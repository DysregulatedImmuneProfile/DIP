.dip_module_to_package <- c(
  numpy = "numpy",
  pandas = "pandas",
  sklearn = "scikit-learn==1.5.2"
)

.ensure_dip_python_deps <- function() {
  required_modules <- names(.dip_module_to_package)
  # Step 1: Try to use modern reticulate  (with uv if possible)
  tryCatch(
    reticulate::py_require(unname(.dip_module_to_package)),
    error = function(e) {
      # Error, we go back to the fallback method
    }
  )

  # Check which modules are still missing
  missing_modules <- required_modules[
    !vapply(required_modules, reticulate::py_module_available, logical(1))
  ]

  # If none are missing, we are done
  if (length(missing_modules) == 0) {
    return(invisible(TRUE))
  }

  packages_to_install <- unname(.dip_module_to_package[missing_modules])

  # Step 2: Fallback (use pip in the active interpreter)
  install_ok <- tryCatch(
    {
      # Join the array of packages into a Python-friendly string, e.g. "'numpy', 'pandas'"
      pkg_string <- paste(sprintf("'%s'", packages_to_install), collapse = ", ")

      # Build the Python script to run
      py_script <- paste0(
        "import subprocess\n",
        "import sys\n",
        "packages = [",
        pkg_string,
        "]\n",
        "for pkg in packages:\n",
        "    subprocess.check_call([sys.executable, '-m', 'pip', 'install', pkg])"
      )

      reticulate::py_run_string(py_script)
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

  # Finale check
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
