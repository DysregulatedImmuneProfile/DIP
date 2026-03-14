.onLoad <- function(libname, pkgname) {
  # Try automatic Python dependency resolution, but never block package load.
  tryCatch(
    reticulate::py_require(c("numpy", "pandas", "scikit-learn==1.5.2")),
    error = function(e) {
      packageStartupMessage(
        paste0(
          "DIP: automatic Python setup failed (",
          conditionMessage(e),
          ").\n",
          "If uv is blocked, set before library(DIP):\n",
          "  Sys.setenv(RETICULATE_USE_UV = '0')\n",
          "  Sys.setenv(RETICULATE_PYTHON = '/path/to/python')\n",
          "Then restart R and try again."
        )
      )
    }
  )
}
