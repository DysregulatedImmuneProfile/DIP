.onLoad <- function(libname, pkgname) {
  # Declare the Python packages your models require
  reticulate::py_require(c(
    "numpy",
    "pandas",
    "scikit-learn==1.5.2"
  ))
}
