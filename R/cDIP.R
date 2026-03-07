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
#' @name cDIP
#' @param new_data A data frame containing ID, TREM_1, IL_6, and Procalcitonin.
#' @return A data frame with the predicted continuous immune dysregulation score (cDIP).
#' @importFrom reticulate use_virtualenv virtualenv_create virtualenv_install py_load_object py_run_string py_config py_available r_to_py
#' @importFrom ggplot2 ggplot aes scale_color_gradient2 theme_minimal expand_limits coord_flip ggtitle
#' @importFrom scales squish
#' @importFrom ggbeeswarm geom_beeswarm
#' @export

cDIP <- function(new_data) {
  Sys.setenv(PIP_DISABLE_PIP_VERSION_CHECK = "1")

  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("ggbeeswarm", quietly = TRUE)
  requireNamespace("reticulate", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  message("This function uses Python. The first-time use might take a few minutes. You might need to restart R afterwards.")

  .fail <- function(...) {
    stop(..., call. = FALSE)
  }

  .norm_path <- function(x) {
    tryCatch(
      normalizePath(x, winslash = "/", mustWork = FALSE),
      error = function(e) x
    )
  }

  package_base_dir <- system.file(package = "DIP")
  if (package_base_dir == "") {
    .fail("Could not locate the installed DIP package directory.")
  }

  model_path <- system.file("extdata/python", "model.pkl", package = "DIP")
  if (!file.exists(model_path)) {
    .fail("Model file not found. Please reinstall the DIP package.")
  }

  # Keep the venv in the package folder
  venv_dir <- file.path(package_base_dir, "r-reticulate-env")

  expected_candidates <- c(
    file.path(venv_dir, "bin", "python"),
    file.path(venv_dir, "Scripts", "python.exe")
  )
  expected_candidates_norm <- unique(vapply(expected_candidates, .norm_path, character(1)))

  if (!dir.exists(venv_dir)) {
    message("Creating a new virtual environment...")
    reticulate::virtualenv_create(envname = venv_dir)
  }

  # If Python is not yet initialized, request the DIP environment.
  # Suppress the harmless reticulate warning about overriding a prior use_python() request.
  if (!reticulate::py_available(initialize = FALSE)) {
    suppressWarnings(
      reticulate::use_virtualenv(venv_dir, required = TRUE)
    )
  }

  cfg <- reticulate::py_config()
  active_python_norm <- .norm_path(cfg$python)

  invisible(
    suppressWarnings(
      suppressMessages(
        capture.output(
          reticulate::virtualenv_install(
            envname = venv_dir,
            packages = c("numpy", "pandas", "scikit-learn==1.5.2"),
            ignore_installed = TRUE
          )
        )
      )
    )
  )

  sklearn_ok <- tryCatch({
    reticulate::py_run_string("import sklearn")
    TRUE
  }, error = function(e) FALSE)

  if (!sklearn_ok) {
    .fail(
      paste0(
        "DIP could not import scikit-learn from the active Python environment.\n",
        "Active interpreter:\n  ", active_python_norm, "\n\n",
        "Expected DIP environment:\n  ", venv_dir, "\n\n",
        "Please restart R and rerun cDIP().\n",
        "If the problem persists, delete this folder and try again:\n  ", venv_dir
      )
    )
  }

  reticulate::py_run_string("
import warnings
from sklearn.exceptions import InconsistentVersionWarning
warnings.filterwarnings('ignore', category=InconsistentVersionWarning)
")

  model <- tryCatch(
    reticulate::py_load_object(model_path),
    error = function(e) {
      .fail(
        paste0(
          "Failed to load the prediction model.\n",
          "Model path:\n  ", model_path, "\n\n",
          "Underlying error:\n", conditionMessage(e)
        )
      )
    }
  )

  if (!is.data.frame(new_data)) {
    .fail("Error: new_data must be a data frame.")
  }
  if (!"ID" %in% names(new_data)) {
    .fail("Error: Data must contain an 'ID' column.")
  }
  if (anyDuplicated(new_data$ID)) {
    .fail("Error: IDs are not unique. Each ID must be unique.")
  }

  expected_vars <- c("Procalcitonin", "TREM_1", "IL_6")
  missing_vars <- setdiff(expected_vars, names(new_data))
  if (length(missing_vars) > 0) {
    .fail("Error: Missing required columns: ", paste(missing_vars, collapse = ", "))
  }

  rows_with_missing <- new_data$ID[rowSums(is.na(new_data[expected_vars])) > 0]
  if (length(rows_with_missing) > 0) {
    message("Patients with missing classifiers are omitted. Affected patient IDs:")
    message(paste(rows_with_missing, collapse = " "))
    new_data <- new_data[!new_data$ID %in% rows_with_missing, , drop = FALSE]
  }

  if (nrow(new_data) == 0) {
    .fail("No rows remain after removing rows with missing classifier data.")
  }

  predictors <- new_data[, expected_vars, drop = FALSE]
  if (!all(vapply(predictors, is.numeric, logical(1)))) {
    .fail("All predictor columns must be numeric.")
  }

  predictors_py <- reticulate::r_to_py(as.data.frame(predictors))

  prediction <- tryCatch(
    model$model$predict(predictors_py),
    error = function(e) .fail("Prediction failed: ", conditionMessage(e))
  )

  prediction <- tryCatch(
    as.numeric(prediction),
    error = function(e) .fail("Prediction output could not be converted to numeric.")
  )

  results_df <- data.frame(
    ID = new_data$ID,
    TREM_1 = new_data$TREM_1,
    IL_6 = new_data$IL_6,
    Procalcitonin = new_data$Procalcitonin,
    cDIP = prediction
  )

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

  print(p)

  if (isTRUE(getOption("cDIP.self_check", TRUE))) {

    .selfcheck_fail <- function(detail) {
      .fail(
        "cDIP self-check failed: the results for the built-in reference cases differ from the expected outputs.\n\n",
        "Detail: ", detail, "\n\n",
        "This indicates a likely package/environment misalignment or a broken/mismatched prediction model file.\n",
        "Suggested actions:\n",
        "  1) Reinstall the DIP package.\n",
        "  2) Restart R.\n",
        "  3) Ensure Python is properly installed and restart R.\n",
        "  4) Recreate the DIP virtual environment and rerun.\n",
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
    pred_sc <- model$model$predict(predictors_py_sc)

    pred_sc_num <- tryCatch(as.numeric(pred_sc), error = function(e) NULL)
    if (is.null(pred_sc_num) || length(pred_sc_num) != 3) {
      .selfcheck_fail("Unexpected prediction output type/length from Python model during self-check.")
    }

    if (any(!is.finite(pred_sc_num))) {
      .selfcheck_fail("Non-finite values (NA/Inf) returned during self-check.")
    }

    tol <- 0.00005
    diffs <- abs(pred_sc_num - self_expected)

    if (any(diffs > tol)) {
      .selfcheck_fail(
        paste0(
          "cDIP mismatch beyond 4-decimal tolerance (±", tol, ").\n",
          "Expected: ", paste(self_expected, collapse = ", "), "\n",
          "Got:      ", paste(pred_sc_num, collapse = ", "), "\n",
          "Abs diff: ", paste(signif(diffs, 6), collapse = ", ")
        )
      )
    }
  }

  assign("cDIP_results", results_df, envir = .GlobalEnv)
  assign("cDIP_plot", p, envir = .GlobalEnv)

  message("Just a reminder: this function expects TREM_1, IL_6, and Procalcitonin in pg/ml (raw, untransformed values).")
  message("Results have been saved to the global environment as 'cDIP_results'.")
  message("The beeswarm plot has been saved as 'cDIP_plot'.")

  invisible(results_df)
}
