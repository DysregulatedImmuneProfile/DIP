#' Shiny-compatible wrapper for DIP_stage()
#' @export
run_DIP_stage_for_shiny <- function(new_data) {
  DIP::DIP_stage(new_data)

  results <- get("DIP_stage_results", envir = .GlobalEnv)
  pie <- get("DIP_stage_piechart", envir = .GlobalEnv)
  scatter <- get("DIP_stage_3D", envir = .GlobalEnv)

  removed <- attr(results, "removed_IDs")  # optional, see note below

  list(
    results = results,
    piechart = pie,
    scatter3d = scatter,
    removed = removed
  )
}

#' Shiny-compatible wrapper for cDIP()
#' @export
run_cDIP_for_shiny <- function(new_data) {
  DIP::cDIP(new_data)

  results <- get("cDIP_results", envir = .GlobalEnv)
  plot <- get("cDIP_plot", envir = .GlobalEnv)
  removed <- attr(results, "removed_IDs")

  list(
    results = results,
    plot = plot,
    removed = removed
  )
}
