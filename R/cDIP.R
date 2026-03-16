#' Predicts the continuous immune dysregulation score (cDIP)
#'
#' This function uses a machine-learning model (random forest regressor) to predict
#' the continuous degree of immune dysregulation (cDIP) using plasma concentrations
#' of sTREM-1, IL-6, and Procalcitonin.
#' The model leverages absolute values to deliver precise, tailored outcomes,
#' suitable for single-patient scenarios.
#' The function automatically omits patients with missing classifier data.
#' Outputs include the original classifier data and the predicted continuous
#' dysregulation score (cDIP), ranging from 0 to 1.
#' A higher cDIP score reflects a greater degree of immune dysregulation.
#' A beeswarm plot is generated to visualize the distribution of cDIP scores.
#' The prediction results and the plot are saved to the global environment.
#'
#'
#' @name cDIP
#' @param new_data A data frame containing ID, TREM_1, IL_6, and Procalcitonin.
#' @return A data frame with the predicted continuous immune dysregulation score (cDIP).
#' @importFrom reticulate py_require py_run_string py_load_object py_config py_available r_to_py py_to_r
#' @importFrom ggplot2 ggplot aes scale_color_gradient2 theme_minimal expand_limits coord_flip ggtitle
#' @importFrom scales squish
#' @importFrom ggbeeswarm geom_beeswarm
#' @export

cDIP <- function(new_data) {
  Sys.setenv(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  
  # Check required R packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The package 'ggplot2' must be installed to use cDIP().", call. = FALSE)
  }
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("The package 'ggbeeswarm' must be installed to use cDIP().", call. = FALSE)
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The package 'reticulate' must be installed to use cDIP().", call. = FALSE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("The package 'scales' must be installed to use cDIP().", call. = FALSE)
  }
  
  # Small internal helpers
  `%||%` <- function(x, y) {
    if (is.null(x) || !nzchar(x)) y else x
  }
  
  .fail <- function(...) {
    stop(..., call. = FALSE)
  }
  
  .norm_path <- function(x) {
    tryCatch(
      normalizePath(x, winslash = "/", mustWork = FALSE),
      error = function(e) x
    )
  }
  
  # Detect system Python candidates
  .detect_system_python <- function() {
    candidates <- c(
      Sys.which("python"),
      Sys.which("python3")
    )
    
    if (.Platform$OS.type == "windows") {
      candidates <- c(
        candidates,
        tryCatch(system2("where", "python", stdout = TRUE, stderr = FALSE), error = function(e) character()),
        tryCatch(system2("where", "python3", stdout = TRUE, stderr = FALSE), error = function(e) character())
      )
    } else {
      candidates <- c(
        candidates,
        tryCatch(system2("which", "python", stdout = TRUE, stderr = FALSE), error = function(e) character()),
        tryCatch(system2("which", "python3", stdout = TRUE, stderr = FALSE), error = function(e) character())
      )
    }
    
    candidates <- unique(trimws(candidates))
    candidates <- candidates[nzchar(candidates)]
    candidates <- candidates[file.exists(candidates)]
    
    list(
      found = length(candidates) > 0,
      paths = candidates
    )
  }
  
  # User guidance when no Python can be found
  .python_install_guidance <- function() {
    paste0(
      "\u26A0\uFE0F The cDIP function requires Python.\n\n",
      "No working Python installation could be found automatically on this computer.\n",
      "This can happen on hospital or university computers with restricted installation permissions or firewall settings.\n\n",
      "Please install Python from:\n",
      "  https://www.python.org/downloads/\n\n",
      "During installation on Windows, make sure you TICK the box:\n",
      "  'Add Python to PATH'\n",
      "before pressing:\n",
      "  'Install Now'\n\n",
      "\u26A0\uFE0F If Python is already installed but RStudio is not linked to it correctly: In RStudio, go to Tools \u2192 Global Options \u2192 Python, click 'Select...', and choose any available Python interpreter from the list. Apply the changes and restart RStudio if prompted.\n\n",
      "After that, restart R and rerun cDIP()."
    )
  }
  
  # User guidance when Python exists but could not be prepared correctly
  .python_linkage_guidance <- function(python_paths = character(),
                                       managed_error = NULL,
                                       active_python = NULL,
                                       required_packages = NULL) {
    path_text <- if (length(python_paths) > 0) {
      paste0("Detected Python installation(s):\n  ", paste(python_paths, collapse = "\n  "), "\n\n")
    } else {
      ""
    }
    
    managed_text <- if (!is.null(managed_error) && nzchar(managed_error)) {
      paste0("Automatic Python setup reported:\n  ", managed_error, "\n\n")
    } else {
      ""
    }
    
    active_text <- if (!is.null(active_python) && nzchar(active_python)) {
      paste0("Python currently linked to this R session:\n  ", active_python, "\n\n")
    } else {
      ""
    }
    
    required_text <- if (!is.null(required_packages) && length(required_packages) > 0) {
      paste0("Required Python packages:\n  ", paste(required_packages, collapse = ", "), "\n\n")
    } else {
      ""
    }
    
    paste0(
      "cDIP could not prepare a usable Python environment.\n\n",
      "In practical terms, this means the prediction model cannot be started on this computer in the current R session.\n\n",
      "Possible reasons include:\n",
      "- Python is not installed\n",
      "- RStudio is linked to the wrong Python interpreter\n",
      "- the required Python packages could not be loaded or installed\n",
      "- this R session is already locked to a Python environment that cDIP cannot use\n",
      "- the first-run package installation could not access the internet or was blocked by system restrictions\n\n",
      managed_text,
      path_text,
      active_text,
      required_text,
      "\u26A0\uFE0F If Python is already installed: In RStudio, go to Tools \u2192 Global Options \u2192 Python, click 'Select...', and choose any available Python interpreter from the list. Apply the changes and restart RStudio if prompted.\n\n",
      "If needed, you can also point reticulate to a specific Python before loading DIP:\n\n",
      "  Sys.setenv(RETICULATE_USE_UV = '0')\n",
      "  Sys.setenv(RETICULATE_USE_MANAGED_VENV = 'no')\n",
      "  Sys.setenv(RETICULATE_PYTHON = '/path/to/python')\n\n",
      "Then restart R and rerun cDIP()."
    )
  }
  
  # Specific message for locked-down hospital / CDW / OneView-like systems
  .restricted_system_guidance <- function(python_paths = character(),
                                          managed_error = NULL,
                                          active_python = NULL,
                                          required_packages = NULL) {
    path_text <- if (length(python_paths) > 0) {
      paste0("Detected Python installation(s):\n  ", paste(python_paths, collapse = "\n  "), "\n\n")
    } else {
      ""
    }
    
    managed_text <- if (!is.null(managed_error) && nzchar(managed_error)) {
      paste0("Automatic Python setup reported:\n  ", managed_error, "\n\n")
    } else {
      ""
    }
    
    active_text <- if (!is.null(active_python) && nzchar(active_python)) {
      paste0("Python currently linked to this R session:\n  ", active_python, "\n\n")
    } else {
      ""
    }
    
    required_text <- if (!is.null(required_packages) && length(required_packages) > 0) {
      paste0("Required Python packages:\n  ", paste(required_packages, collapse = ", "), "\n\n")
    } else {
      ""
    }
    
    paste0(
      "cDIP could not start a usable Python environment on this computer.\n\n",
      "This commonly occurs on tightly restricted hospital, CDW, OneView, or university systems where users are not allowed to create or modify Python environments.\n\n",
      "In that situation, cDIP will usually not work unless one of the following is already true:\n",
      "- R is already linked to a working Python interpreter\n",
      "- that Python can import numpy, pandas, and scikit-learn\n",
      "- the system allows reticulate to create and manage its Python environment\n\n",
      managed_text,
      path_text,
      active_text,
      required_text,
      "Recommended next steps:\n",
      "  1) If Python is already installed, in RStudio go to Tools -> Global Options -> Python -> Select... and choose a Python interpreter.\n",
      "  2) Restart R and rerun cDIP().\n",
      "  3) If this is a managed hospital/CDW/OneView environment, local installation of Python packages may simply be blocked by policy.\n",
      "  4) In that case, please use a personal/research machine with Python support, or ask ICT whether a working Python environment with numpy, pandas, and scikit-learn can be made available.\n\n",
      "cDIP does not continue with alternative Python setup routes in the same R session, because reticulate can become locked to the first Python strategy that was attempted."
    )
  }
  
  # Validate biomarker input columns
  .validate_predictors <- function(df, expected_vars) {
    rows_with_missing <- df$ID[rowSums(is.na(df[expected_vars])) > 0]
    if (length(rows_with_missing) > 0) {
      message("Patients with missing classifiers are omitted. Affected patient IDs:")
      message(paste(rows_with_missing, collapse = " "))
      df <- df[!df$ID %in% rows_with_missing, , drop = FALSE]
    }
    
    if (nrow(df) == 0) {
      .fail("No predictions could be made because all rows had at least one missing required biomarker value.")
    }
    
    predictors <- df[, expected_vars, drop = FALSE]
    
    if (!all(vapply(predictors, is.numeric, logical(1)))) {
      .fail("One or more required biomarker columns are not numeric. Please ensure TREM_1, IL_6, and Procalcitonin contain numbers only.")
    }
    
    if (any(!is.finite(as.matrix(predictors)))) {
      .fail("Biomarker values must be finite numbers. NA, NaN, or Inf values are not allowed.")
    }
    
    if (any(as.matrix(predictors) < 0)) {
      .fail("Negative biomarker values were detected. TREM_1, IL_6, and Procalcitonin must be non-negative non-scaled concentrations in pg/ml.")
    }
    
    list(data = df, predictors = predictors)
  }
  
  # Read Python package versions for diagnostics
  .get_python_module_versions <- function() {
    tryCatch({
      reticulate::py_run_string("
import sys
import numpy, pandas, sklearn
py_module_versions = {
    'python': sys.version.split()[0],
    'numpy': numpy.__version__,
    'pandas': pandas.__version__,
    'sklearn': sklearn.__version__
}
")
      reticulate::py_to_r(reticulate::py$py_module_versions)
    }, error = function(e) NULL)
  }

# Format Python stack information for error messages only
.python_stack_text <- function(python_versions) {
  reticulate_ver <- tryCatch(
    as.character(utils::packageVersion("reticulate")),
    error = function(e) NULL
  )
  
  if (is.null(python_versions) && is.null(reticulate_ver)) {
    return("")
  }
  
  lines <- c("Detected Python stack:")
  
  if (!is.null(python_versions)) {
    python_ver  <- python_versions[["python"]]
    numpy_ver   <- python_versions[["numpy"]]
    pandas_ver  <- python_versions[["pandas"]]
    sklearn_ver <- python_versions[["sklearn"]]
    
    if (!is.null(python_ver) && nzchar(python_ver)) {
      lines <- c(lines, paste0("  Python ", python_ver))
    }
    if (!is.null(numpy_ver) && nzchar(numpy_ver)) {
      lines <- c(lines, paste0("  numpy ", numpy_ver))
    }
    if (!is.null(pandas_ver) && nzchar(pandas_ver)) {
      lines <- c(lines, paste0("  pandas ", pandas_ver))
    }
    if (!is.null(sklearn_ver) && nzchar(sklearn_ver)) {
      lines <- c(lines, paste0("  scikit-learn ", sklearn_ver))
    }
  }
  
  if (!is.null(reticulate_ver) && nzchar(reticulate_ver)) {
    lines <- c(lines, paste0("  reticulate ", reticulate_ver))
  }
  
  paste0("\n", paste(lines, collapse = "\n"), "\n")
}

# Internal reference validation for sklearn-version-agnostic compatibility
.run_internal_validation <- function(model, expected_vars, python_versions = NULL) {
  .selfcheck_fail <- function(detail) {
    stack <- .python_stack_text(python_versions)
    
    .fail(
      "cDIP internal quality control failed.\n\n",
      "The built-in reference test cases did not match the expected results closely enough.\n\n",
      "Detail: ", detail, "\n",
      stack, "\n",
      "This means the package installation or Python environment is not aligned correctly, so the model output should not be trusted yet.\n",
      "Recommended next steps:\n",
      "  1) Restart R and rerun.\n",
      "  2) If Python is already installed but not linked correctly, in RStudio go to Tools -> Global Options -> Python -> Select... and choose a Python interpreter.\n",
      "  3) If needed, reinstall the DIP package.\n",
      "  4) If needed, reinstall Python from https://www.python.org/downloads/ and restart R.\n",
      "If the problem persists, please report your R version, Python version, reticulate version, and scikit-learn version."
    )
  }
  
  self_test_data <- data.frame(
    ID = 1:3,
    TREM_1 = c(182, 400, 1000),
    IL_6 = c(70, 5, 10000),
    Procalcitonin = c(877, 66, 20000)
  )
  
  self_expected <- c(0.40078689, 0.08400504, 0.94155332)
  
  predictors_sc <- self_test_data[, expected_vars, drop = FALSE]
  predictors_py_sc <- reticulate::r_to_py(as.data.frame(predictors_sc))
  
  pred_sc <- tryCatch(
    model$model$predict(predictors_py_sc),
    error = function(e) .selfcheck_fail(
      paste0("The internal reference test could not be executed. Python reported: ", conditionMessage(e))
    )
  )
  
  pred_sc_num <- tryCatch(
    as.numeric(reticulate::py_to_r(pred_sc)),
    error = function(e) NULL
  )
  
  if (is.null(pred_sc_num) || length(pred_sc_num) != 3) {
    .selfcheck_fail("The internal reference test returned an unexpected type or number of predictions.")
  }
  
  if (any(!is.finite(pred_sc_num))) {
    .selfcheck_fail("The internal reference test returned one or more invalid values (NA or Inf).")
  }
  
  tol <- 0.00005
  diffs <- abs(pred_sc_num - self_expected)
  
  if (any(diffs > tol)) {
    .selfcheck_fail(
      paste0(
        "The internal reference predictions were outside the allowed tolerance.\n",
        "Expected: ", paste(self_expected, collapse = ", "), "\n",
        "Observed: ", paste(signif(pred_sc_num, 8), collapse = ", "), "\n",
        "Absolute difference: ", paste(signif(diffs, 6), collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}

# Locate installed package resources
package_base_dir <- system.file(package = "DIP")
if (package_base_dir == "") {
  .fail("The installed DIP package folder could not be located. Please reinstall the DIP package and try again.")
}

model_path <- system.file("extdata/python", "model.pkl", package = "DIP")
if (!file.exists(model_path)) {
  .fail("The cDIP model file could not be found inside the DIP package. Please reinstall the DIP package and try again.")
}

# Validate top-level user input
if (!is.data.frame(new_data)) {
  .fail("Input data must be provided as a data frame.")
}
if (!"ID" %in% names(new_data)) {
  .fail("The input data must contain a column named 'ID' so each patient or sample can be identified.")
}
if (anyDuplicated(new_data$ID)) {
  .fail("The values in the 'ID' column are not unique. Each patient or sample must have a unique ID.")
}

expected_vars <- c("Procalcitonin", "TREM_1", "IL_6")
missing_vars <- setdiff(expected_vars, names(new_data))
if (length(missing_vars) > 0) {
  .fail("The input data are missing required column(s): ", paste(missing_vars, collapse = ", "), ". Please provide ID, TREM_1, IL_6, and Procalcitonin.")
}

required_packages <- c("numpy", "pandas", "scikit-learn==1.5.2")

python_ok <- FALSE
managed_error <- NULL
system_python <- NULL

message("cDIP is initializing Python dependencies. On first use, this may take a few minutes.")

# 1) If Python is already active in this R session, only validate it.
# Do NOT switch to another Python strategy afterwards.
if (reticulate::py_available(initialize = FALSE)) {
  message("Using already active Python interpreter...")
  
  python_ok <- tryCatch({
    reticulate::py_run_string("import numpy, pandas, sklearn")
    TRUE
  }, error = function(e) {
    cfg <- tryCatch(reticulate::py_config(), error = function(err) NULL)
    active_python_norm <- if (!is.null(cfg)) .norm_path(cfg$python) else "<unavailable>"
    
    .fail(
      paste0(
        "A Python interpreter is already active in this R session, but cDIP cannot use it.\n\n",
        "Current interpreter:\n  ", active_python_norm, "\n\n",
        "Reason reported by Python:\n  ", conditionMessage(e), "\n\n",
        "\u26A0\uFE0F Because reticulate is already linked to this Python interpreter, cDIP will not attempt a different Python setup route in the same R session.\n\n",
        "\u26A0\uFE0F If Python is already installed: In RStudio, go to Tools \u2192 Global Options \u2192 Python, click 'Select...', and choose any available Python interpreter from the list. Apply the changes and restart RStudio if prompted.\n\n",
        "Or before loading DIP, set:\n",
        "  Sys.setenv(RETICULATE_PYTHON = '/path/to/python')\n\n",
        "Then restart R and rerun cDIP()."
      )
    )
  })
}

# 2) If no Python is active yet, try ONE managed-Python route only.
# If this fails, stop with guidance; do not fall back to another strategy in the same session.
if (!python_ok && !reticulate::py_available(initialize = FALSE)) {
  old_reticulate_python <- Sys.getenv("RETICULATE_PYTHON", unset = NA_character_)
  
  on.exit({
    if (is.na(old_reticulate_python)) {
      Sys.unsetenv("RETICULATE_PYTHON")
    } else {
      Sys.setenv(RETICULATE_PYTHON = old_reticulate_python)
    }
  }, add = TRUE)
  
  Sys.setenv(RETICULATE_PYTHON = "managed")
  
  python_ok <- tryCatch({
    reticulate::py_require(required_packages)
    reticulate::py_run_string("import numpy, pandas, sklearn")
    message("DIP is using a managed Python environment.")
    TRUE
  }, error = function(e) {
    managed_error <<- conditionMessage(e)
    FALSE
  })
  
  if (!python_ok) {
    cfg <- tryCatch(reticulate::py_config(), error = function(err) NULL)
    active_python_norm <- if (!is.null(cfg)) .norm_path(cfg$python) else NULL
    system_python <- .detect_system_python()
    
    if (!system_python$found) {
      .fail(.python_install_guidance())
    }
    
    .fail(
      .restricted_system_guidance(
        python_paths = system_python$paths,
        managed_error = managed_error,
        active_python = active_python_norm,
        required_packages = required_packages
      )
    )
  }
}

# Final guard
if (!python_ok) {
  cfg <- tryCatch(reticulate::py_config(), error = function(err) NULL)
  active_python_norm <- if (!is.null(cfg)) .norm_path(cfg$python) else NULL
  system_python <- .detect_system_python()
  
  .fail(
    .python_linkage_guidance(
      python_paths = if (!is.null(system_python)) system_python$paths else character(),
      managed_error = managed_error,
      active_python = active_python_norm,
      required_packages = required_packages
    )
  )
}

# Silence sklearn version warning because compatibility is checked by internal validation
reticulate::py_run_string("
import warnings
from sklearn.exceptions import InconsistentVersionWarning
warnings.filterwarnings('ignore', category=InconsistentVersionWarning)
")

# Load the serialized prediction model
model <- tryCatch(
  reticulate::py_load_object(model_path),
  error = function(e) {
    .fail(
      paste0(
        "The cDIP prediction model could not be loaded.\n\n",
        "Model file:\n  ", model_path, "\n\n",
        "Reason reported by Python:\n", conditionMessage(e),
        .python_stack_text(.get_python_module_versions()), "\n",
        "This usually means the package installation is incomplete, the model file is incompatible with the current Python environment, or required Python packages are not correctly installed.\n",
        "Please restart R and try again. If the problem persists, reinstall the DIP package."
      )
    )
  }
)

# Retrieve Python module versions for diagnostics
python_versions <- .get_python_module_versions()

# Internal validation happens BEFORE real prediction, plotting, or global assignment
if (isTRUE(getOption("cDIP.self_check", TRUE))) {
  .run_internal_validation(
    model = model,
    expected_vars = expected_vars,
    python_versions = python_versions
  )
}

# Validate user-provided biomarker inputs
checked <- .validate_predictors(new_data, expected_vars)
new_data <- checked$data
predictors <- checked$predictors

# Convert predictors to Python format
predictors_py <- reticulate::r_to_py(as.data.frame(predictors))

# Generate predictions using the trained model
prediction <- tryCatch(
  model$model$predict(predictors_py),
  error = function(e) .fail(
    "The cDIP model could not generate predictions from the provided data. ",
    "Please confirm that TREM_1, IL_6, and Procalcitonin are numeric and supplied as raw values in pg/ml.\n",
    "Technical detail: ", conditionMessage(e),
    .python_stack_text(python_versions)
  )
)

# Convert prediction output back to R numeric values
prediction <- tryCatch(
  as.numeric(reticulate::py_to_r(prediction)),
  error = function(e) {
    .fail(
      "The prediction results were generated by Python but could not be read correctly in R. ",
      "Please restart R and try again. If the problem persists, reinstall the DIP package and check the Python setup.",
      .python_stack_text(python_versions)
    )
  }
)

# Post-prediction sanity check
if (length(prediction) != nrow(new_data)) {
  .fail(
    "The cDIP model returned an unexpected number of predictions. ",
    "Please restart R and try again. If the problem persists, reinstall the DIP package and check the Python setup.",
    .python_stack_text(python_versions)
  )
}

if (any(!is.finite(prediction))) {
  .fail(
    "The cDIP model returned one or more invalid prediction values (NA, NaN, or Inf). ",
    "Please restart R and try again. If the problem persists, reinstall the DIP package and check the Python setup.",
    .python_stack_text(python_versions)
  )
}

# Assemble the final results table
results_df <- data.frame(
  ID = new_data$ID,
  TREM_1 = new_data$TREM_1,
  IL_6 = new_data$IL_6,
  Procalcitonin = new_data$Procalcitonin,
  cDIP = prediction
)

# Generate beeswarm plot of cDIP score distribution
p <- ggplot2::ggplot(results_df, ggplot2::aes(y = cDIP, x = factor(1), color = cDIP)) +
  ggbeeswarm::geom_beeswarm(cex = 3, method = "swarm", size = 1, dodge.width = 0.5) +
  ggplot2::scale_color_gradient2(
    low = "#8BCCF1",
    mid = "#896DB0",
    high = "#006E78",
    midpoint = 0.5,
    limits = c(0, 1),
    oob = scales::squish
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 1.5)) +
  ggplot2::coord_flip() +
  ggplot2::ggtitle("cDIP distribution in your cohort") +
  ggplot2::expand_limits(y = c(0, 1)) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    panel.border = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 12, face = "italic"),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 8),
    legend.text = ggplot2::element_text(size = 8)
  )

# Print plot for interactive use
print(p)

# Save outputs to the global environment
assign("cDIP_results", results_df, envir = .GlobalEnv)
assign("cDIP_plot", p, envir = .GlobalEnv)

# Final user messages
message("Just a reminder: this function expects TREM_1, IL_6, and Procalcitonin in pg/ml (raw, untransformed values).")
message("Results have been saved to the global environment as 'cDIP_results'.")
message("The beeswarm plot has been saved as 'cDIP_plot'.")

invisible(results_df)
}
