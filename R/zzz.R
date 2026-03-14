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
  missing_modules <- names(.dip_module_to_package)[
    !vapply(
      names(.dip_module_to_package),
      reticulate::py_module_available,
      logical(1)
    )
  ]

  if (length(missing_modules) == 0) {
    return(invisible(TRUE))
  }

  packages_to_install <- unname(.dip_module_to_package[missing_modules])

  install_ok <- tryCatch(
    {
      reticulate::py_install(packages_to_install, pip = TRUE)
      TRUE
    },
    error = function(e) {
      message(
        "DIP could not auto-install missing Python packages (",
        conditionMessage(e),
        ")."
      )
      FALSE
    }
  )

  if (!install_ok) {
    return(invisible(FALSE))
  }

  still_missing <- names(.dip_module_to_package)[
    !vapply(
      names(.dip_module_to_package),
      reticulate::py_module_available,
      logical(1)
    )
  ]

  invisible(length(still_missing) == 0)
}

.onLoad <- function(libname, pkgname) {
  # Declare Python package requirements for automatic resolution.
  # In restricted environments this may fail; cDIP() will also attempt
  # to install missing packages in the selected Python environment.
  tryCatch(
    reticulate::py_require(.dip_python_requirements),
    error = function(e) {
      packageStartupMessage(
        "DIP: automatic Python dependency setup was not completed (",
        conditionMessage(e),
        ")."
      )
    }
  )
}
