cDIP <- function(new_data) {
  # Suppress pip upgrade messages
  Sys.setenv(PIP_DISABLE_PIP_VERSION_CHECK = "1")

  # Ensure required packages are loaded properly
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("ggbeeswarm", quietly = TRUE)
  requireNamespace("reticulate", quietly = TRUE)

  message("Please ensure TREM_1, IL_6, and Procalcitonin are in pg/ml, untransformed and unscaled.")
  message("This function uses Python. The first-time use might take a few minutes. You might need to restart R afterwards.")

  # Define virtual environment directory
  package_base_dir <- system.file(package = "DIP")
  venv_dir <- file.path(package_base_dir, "r-reticulate-env")

  # Ensure the virtual environment exists
  if (!dir.exists(venv_dir)) {
    message("Creating a new virtual environment...")
    reticulate::virtualenv_create(envname = venv_dir)
  }

  # Activate the virtual environment
  reticulate::use_virtualenv(venv_dir, required = TRUE)

  # Install correct scikit-learn version to match model
  reticulate::virtualenv_install(envname = venv_dir, packages = "scikit-learn==1.5.2", ignore_installed = TRUE)

  # Ensure Python dependencies are available
  required_packages <- c("numpy", "pandas", "scikit-learn")
  for (pkg in required_packages) {
    if (!reticulate::py_module_available(pkg)) {
      message(sprintf("Installing Python package: %s", pkg))
      reticulate::virtualenv_install(envname = venv_dir, packages = pkg)
    }
  }

  # Load the Python model
  model_path <- system.file("extdata/python", "model.pkl", package = "DIP")
  if (!file.exists(model_path)) {
    stop("Model file not found. Please check the package installation.")
  }

  model <- reticulate::py_load_object(model_path)

  # Validate input data
  if (!is.data.frame(new_data)) {
    stop("Error: new_data must be a data frame.")
  }
  if (!"ID" %in% names(new_data)) {
    stop("Error: Data must contain an 'ID' column.")
  }
  if (anyDuplicated(new_data$ID)) {
    stop("Error: IDs are not unique. Each ID must be unique.")
  }

  # Ensure required columns are present
  expected_vars <- c("Procalcitonin", "TREM_1", "IL_6")
  missing_vars <- setdiff(expected_vars, names(new_data))
  if (length(missing_vars) > 0) {
    stop("Error: Missing required columns: ", paste(missing_vars, collapse = ", "))
  }

  # Remove rows with missing classifier data
  rows_with_missing <- new_data$ID[rowSums(is.na(new_data[expected_vars])) > 0]
  if (length(rows_with_missing) > 0) {
    message("Patients with missing classifiers are omitted. Affected patient IDs:")
    message(paste(rows_with_missing, collapse = " "))
    new_data <- new_data[!new_data$ID %in% rows_with_missing, ]
  }

  # Prepare predictor data
  predictors <- new_data[, expected_vars]
  if (!all(sapply(predictors, is.numeric))) {
    stop("All predictor columns must be numeric.")
  }

  # Convert predictor data to Python-readable format
  predictors <- as.data.frame(predictors)  # Ensure it's a data frame
  predictors_py <- reticulate::r_to_py(predictors)  # Convert to Python object

  # Run the model prediction
  prediction <- model$model$predict(predictors_py)

  # Create results dataframe
  results_df <- data.frame(
    ID = new_data$ID,
    TREM_1 = new_data$TREM_1,
    IL_6 = new_data$IL_6,
    Procalcitonin = new_data$Procalcitonin,
    cDIP = prediction
  )

  # Create beeswarm plot
  p <- ggplot2::ggplot(results_df, ggplot2::aes(y = cDIP, x = factor(1), color = cDIP)) +
    ggbeeswarm::geom_beeswarm(cex = 3, method = "hex", size = 1, dodge.width = 0.5) +
    ggplot2::scale_color_gradient2(
      low = "#8BCCF1",
      mid = "#896DB0",
      high = "#006E78",
      midpoint = 0.5,
      limits = c(0, 1),  # Forces legend to include 0 and 1
      oob = scales::squish  # Ensures values outside limits are clamped
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

  print(p)  # Display the plot


  # Save results and plot to the global environment
  assign("cDIP_results", results_df, envir = .GlobalEnv)
  assign("cDIP_plot", p, envir = .GlobalEnv)

  # Message user about saved objects
  message("Results have been saved to the global environment as 'cDIP_results'.")
  message("The beeswarm plot has been saved as 'cDIP_plot'.")

  # Return results invisibly (so they are NOT printed)
  invisible(results_df)
}
