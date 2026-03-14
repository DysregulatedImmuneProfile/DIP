.dip_python_requirements <- c(
  "numpy",
  "pandas",
  "scikit-learn==1.5.2"
)

.dip_module_to_package <- c(
  numpy = "numpy",
  pandas = "pandas",
  sklearn = "scikit-learn==1.5.2"
)

.ensure_dip_python_deps <- function() {
  required_modules <- names(.dip_module_to_package)

  missing_modules <- required_modules[
    !vapply(required_modules, reticulate::py_module_available, logical(1))
  ]

  if (length(missing_modules) == 0) {
    return(invisible(TRUE))
  }

  packages_to_install <- unname(.dip_module_to_package[missing_modules])

  install_ok <- tryCatch(
    {
      reticulate::py$dip_packages_to_install <- packages_to_install
      reticulate::py_run_string(
        "import subprocess, sys\nfor pkg in dip_packages_to_install:\n    subprocess.check_call([sys.executable, '-m', 'pip', 'install', pkg])"
      )
      TRUE
    },
    error = function(e) {
      message(
        "DIP could not auto-install missing Python packages in the active Python interpreter (",
        conditionMessage(e),
        ")."
      )
      FALSE
    }
  )

  if (!install_ok) {
    return(invisible(FALSE))
  }

  still_missing <- required_modules[
    !vapply(required_modules, reticulate::py_module_available, logical(1))
  ]

  invisible(length(still_missing) == 0)
}

.onLoad <- function(libname, pkgname) {
  # Avoid selecting/initializing Python at package load time.
  # Python dependency resolution is performed lazily in cDIP().
  invisible(NULL)
}
