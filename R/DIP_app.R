#' Launch the DIP Shiny Application
#'
#' This launches the graphical user interface for predicting DIP stage and cDIP scores.
#' @export
DIP_app <- function() {
  app_dir <- system.file("shiny_app", package = "DIP")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the DIP package.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}
