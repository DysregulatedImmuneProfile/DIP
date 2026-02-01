#' Predicts the stage of host response dysregulation: Dysregulated Immune Profile 1-3
#'
#' This function uses an extreme XGBoost model to predict the degree of host response dysregulation using plasma concentrations of sTREM-1, IL-6, and Procalcitonin.
#' The prediction model leverages absolute values to deliver precise, tailored outcomes, suitable for single-patient scenarios.
#' The function automatically omits patients with missing classifier data.
#' Outputs include the original classifier data, the predicted DIP stage, and probability scores for each predicted stage.
#' DIP1 represents minor, DIP2 moderate and DIP3 major host response dysregulation.
#' A pie chart (using plotly) is generated for the distribution of predictions, and an interactive 3D scatter plot is also created.
#' The prediction results and both plots are saved to the global environment.
#'
#' @name DIP_stage
#' @param new_data A data frame containing the following columns:
#' \itemize{
#'   \item \code{ID}: unique identifier for the observations. Patients with multiple timepoints should have the timepoint included in their ID.
#'   \item \code{TREM_1}: sTREM-1 measured in picograms per milliliter (pg/ml). Data should be untransformed and unscaled. The column name must be exactly \code{TREM_1}.
#'   \item \code{IL_6}: IL-6 measured in picograms per milliliter (pg/ml). Data should be untransformed and unscaled. The column name must be exactly \code{IL_6}.
#'   \item \code{Procalcitonin}: Procalcitonin measured in picograms per milliliter (pg/ml). Data should be untransformed and unscaled. The column name must be exactly \code{Procalcitonin}.
#' }
#' @return A data frame with predicted DIP stage and probabilities:
#' #' \itemize{
#'   \item \code{ID}: Unique identifier for each observation.
#'   \item \code{DIP}: Predicted DIP stage (DIP1, DIP2, or DIP3).
#'   \item \code{DIP1_Prob}, \code{DIP2_Prob}, \code{DIP3_Prob}: Prediction probabilities for each stage.
#' }
#' @import xgboost
#' @importFrom plotly plot_ly layout
#' @importFrom scales squish
#' @export
#' @examples
#' test_data <- data.frame(
#'   ID = 1:20,
#'   TREM_1 = c(182, 400, 1000, 560, 230, 900, 450, 710, 620, 350,
#'              150, 800, 250, 490, 780, 340, 900, 1100, 220, 510),
#'   IL_6 = c(70, 5, 10000, 450, 88, 3000, 150, 680, 740, 50,
#'            30, 600, 120, 470, 800, 60, 5000, 9000, 33, 200),
#'   Procalcitonin = c(877, 66, 20000, 1500, 500, 10000, 800, 2700, 1800, 460,
#'                     250, 12000, 600, 1100, 14000, 350, 15000, 18000, 310, 900)
#' )
#' DIP_stage(test_data)
DIP_stage <- function(new_data) {
  ## Load the model from the package's internal directory
  model_path <- system.file("extdata", "xgb_model.json", package = "DIP")
  if (!file.exists(model_path)) {
    stop("Model file not found in package. Please reinstall the package and restart R.")
  }
  model <- xgboost::xgb.load(model_path)

  message("Please ensure TREM_1, IL_6, and Procalcitonin are in pg/ml, untransformed and unscaled.")

  ## Validate new_data
  if (nrow(new_data) == 0) {
    stop("Error: Input data frame is empty. Provide valid patient data.")
  }

  if (!is.data.frame(new_data)) {
    stop("Error: new_data must be a data frame.")
  }
  if (!"ID" %in% names(new_data)) {
    stop("Error: Data must contain an 'ID' column.")
  }
  if (anyDuplicated(new_data$ID) > 0) {
    stop("Error: IDs are not unique. Each ID must be unique. Patients with multiple timepoints should have the timepoint included in their ID.")
  }


  ## Ensure the predictor data is correct
  expected_vars <- c("TREM_1", "IL_6", "Procalcitonin")  # Correct order expected by the model

  missing_vars <- setdiff(expected_vars, names(new_data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Error: Missing required columns: %s. Ensure these exact names are used in your dataset.", paste(missing_vars, collapse = ", ")))
  }

  ## Check if columns are in the correct order
  if (!identical(names(new_data)[match(expected_vars, names(new_data))], expected_vars)) {
    warning("Column order in the dataset does not match the model's expected format. Reordering to match the correct order.")
    new_data <- new_data[, c("ID", expected_vars)]
  }

  ## Run consistency checks if more than 10 patients are provided
  if (nrow(new_data) > 10) {
    for (var in expected_vars) {
      freqs <- table(new_data[[var]])
      if (length(freqs) > 0) {
        max_freq <- max(freqs)
        percent_max_freq <- (max_freq / nrow(new_data)) * 100
        if (percent_max_freq > 10) {
          warning(sprintf("Warning: More than 10%% of %s are the exact same value (%.2f%%).", var, percent_max_freq))
        }
      }
    }
  }

  ## Omit patients with missing classifier data
  rows_with_missing <- new_data$ID[rowSums(is.na(new_data[expected_vars])) > 0]
  if (length(rows_with_missing) > 0) {
    message("Patients with missing classifier data are omitted. Affected patient IDs:")
    message(paste(rows_with_missing, collapse = " "))
    new_data <- new_data[!new_data$ID %in% rows_with_missing, ]
  }

  ## Extract predictor columns in the correct order and convert factors to numeric if needed
  predictors <- new_data[, expected_vars]
  predictors[] <- lapply(predictors, function(x) if (is.factor(x)) as.numeric(as.factor(x)) else x)
  if (!all(sapply(predictors, is.numeric))) {
    stop("All biomarker columns in new_data must be numeric.")
  }

  ## Convert predictor data to matrix and create a DMatrix object
  predictors_matrix <- as.matrix(predictors)
  dmatrix <- xgboost::xgb.DMatrix(predictors_matrix)

  ## Generate predictions using the loaded model
  predictions <- predict(model, dmatrix, predcontrib = FALSE)
  num_classes <- 3  # Ensure this matches your model's configuration
  predicted_classes <- matrix(predictions, ncol = num_classes, byrow = TRUE)

  if (any(is.na(predictions))) {
    stop("Error: Model prediction returned NA values. Check input data format.")
  }

  ## Determine the predicted class for each observation
  max_indices <- apply(predicted_classes, 1, which.max)
  labels <- c("DIP1", "DIP2", "DIP3")
  labeled_predictions <- labels[max_indices]

  ## Create a results data frame
  results_df <- data.frame(
    ID = new_data$ID,
    DIP = labeled_predictions,
    DIP1_Prob = predicted_classes[, 1],
    DIP2_Prob = predicted_classes[, 2],
    DIP3_Prob = predicted_classes[, 3]
  )

  ## Prepare data for the pie chart
  prediction_counts <- table(results_df$DIP)
  prediction_data <- as.data.frame(prediction_counts)
  names(prediction_data) <- c("DIP", "Freq")
  prediction_data$percentage <- round(prediction_data$Freq / sum(prediction_data$Freq) * 100, 1)
  prediction_data$fill_label <- paste(prediction_data$DIP, paste0("(", prediction_data$percentage, "%)"))

  ## Define custom colors
  custom_colors <- c("#8BCCF1", "#896DB0", "#006E78")

  ## Create the pie chart using plotly
  pie_chart <- plotly::plot_ly(
    data = prediction_data,
    labels = ~fill_label,
    values = ~Freq,
    type = 'pie',
    textinfo = 'label+percent',
    marker = list(colors = custom_colors))

  ## Apply layout separately
  pie_chart <- plotly::layout(pie_chart, title = "Distribution of DIP Predictions")

  ## Print the pie chart
  print(pie_chart)

  ## Create the 3D scatter (dot) plot using plotly
  unique_DIP <- unique(results_df$DIP)
  color_map <- setNames(custom_colors[seq_along(unique_DIP)], unique_DIP)

  scatter_3d <- plotly::plot_ly(
    data = results_df,
    x = ~DIP1_Prob * 100,
    y = ~DIP2_Prob * 100,
    z = ~DIP3_Prob * 100,
    alpha = 1,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 2),
    color = ~DIP,
    colors = custom_colors
  )

  scatter_3d <- plotly::layout(scatter_3d,
    scene = list(
      xaxis = list(title = "DIP1 Prob (%)"),
      yaxis = list(title = "DIP2 Prob (%)"),
      zaxis = list(title = "DIP3 Prob (%)")
    ),
    title = "3D Scatter Plot of DIP Predictions"
  )

## Sanity check: abort if all DIP proportions are highly similar (between 30% and 36%) as this is biologically inplausible 
n <- nrow(results_df)

# Fixed-order counts so missing classes become 0 instead of disappearing
prediction_counts <- table(factor(results_df$DIP, levels = c("DIP1", "DIP2", "DIP3")))
props <- as.numeric(prediction_counts) / n

if (all(props >= 0.30 & props <= 0.36)) {
    stop(
      "Invalid input detected: the predicted DIP distribution is approximately ",
      "1/3–1/3–1/3 across DIP1/DIP2/DIP3.\n\n",
      "This pattern is highly unlikely for valid biological input and usually ",
      "indicates malformed data or an execution/environment issue.\n\n",
      "Please verify:\n",
      "  - Biomarker values are raw pg/ml (not scaled, normalized, or transformed).\n",
      "  - Biomarker columns are truly numeric (not factors/characters).\n",
      "  - Decimal separator is '.' (not ',').\n",
      "  - Required packages in the DIP environment (e.g. dplyr, reticulate) ",
      "    are not being masked or overwritten.\n",
      "No results were returned."
    )
}

  ## Save the results and both plots in the global environment
  assign("DIP_stage_results", results_df, envir = .GlobalEnv)
  assign("DIP_stage_piechart", pie_chart, envir = .GlobalEnv)
  assign("DIP_stage_3D", scatter_3d, envir = .GlobalEnv)

  message("Results have been saved to the global environment as 'DIP_stage_results'.")
  message("The pie chart is saved as 'DIP_stage_piechart'.")
  message("The interactive 3D scatter plot with DIP probabilities is saved as 'DIP_stage_3D'.")

  invisible(results_df)
}

