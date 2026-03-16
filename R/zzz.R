# zzz.R — Python dependency management for the DIP package
#
# This file defines the required Python packages and the helper functions
# that resolve them at runtime. Dependency resolution is performed lazily
# (inside cDIP(), not at package load) to avoid locking the R session to
# a Python interpreter before the user has had a chance to configure one.
#
# Resolution order:
#   1. reticulate::py_require() via uv          — preferred, fully automatic
#   2. pip install in the active interpreter    — fallback for restricted/
#                                                 hospital environments

# Used for py_require() — simple names only, avoids duplicate/conflict with
# reticulate's own internal numpy requirement
.dip_py_require_packages <- c(
  "numpy",
  "pandas",
  "scikit-learn==1.5.2" # keep pin here since this is safety-critical
)

# Used for the pip fallback — full version bounds for conflict prevention
.dip_module_to_package <- c(
  numpy = "numpy>=1.21,<2.0",
  pandas = "pandas>=1.3,<3.0",
  sklearn = "scikit-learn==1.5.2"
)

.dip_required_modules <- names(.dip_module_to_package)

.dip_python_import_status <- function() {
  # Try to run a simple script that explicitly imports everything.
  # Returns both status and error message for better diagnostics.
  tryCatch(
    {
      import_code <- paste(
        sprintf("import %s", .dip_required_modules),
        collapse = "\n"
      )
      reticulate::py_run_string(import_code)
      list(ok = TRUE, error = NULL)
    },
    error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    }
  )
}

.dip_python_imports_ok <- function() {
  isTRUE(.dip_python_import_status()$ok)
}

.ensure_dip_python_deps <- function() {
  py_already_active <- reticulate::py_available(initialize = FALSE)
  use_uv <- Sys.getenv("RETICULATE_USE_UV", unset = "1") != "0"

  # STEP 1: If Python is NOT active, prioritize uv via py_require FIRST.
  # Do this BEFORE checking import status, so we don't accidentally initialize a broken Python.
  if (!py_already_active && use_uv) {
    uv_error <- NULL
    tryCatch(
      reticulate::py_require(.dip_py_require_packages),
      error = function(e) {
        uv_error <<- conditionMessage(e)
      }
    )
  }

  # STEP 2: Now that the environment is requested/built, check if imports actually work.
  import_status <- .dip_python_import_status()
  if (isTRUE(import_status$ok)) {
    return(invisible(TRUE))
  }

  # STEP 3: Fallback - use pip in the active interpreter
  packages_to_install <- unname(.dip_module_to_package)

  install_ok <- tryCatch(
    {
      pkg_string <- paste(sprintf("'%s'", packages_to_install), collapse = ", ")

      # Build the Python script to run.
      # Notice the try/except block that automatically bootstraps pip if it is missing!
      py_script <- paste0(
        "import subprocess\n",
        "import sys\n",
        "try:\n",
        "    import pip\n",
        "except ImportError:\n",
        "    try:\n",
        "        import ensurepip\n",
        "        ensurepip.bootstrap()\n",
        "    except Exception as e:\n",
        "        raise RuntimeError(f\"pip is not available and ensurepip bootstrap failed: {e}\")\n",
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

  # Final check: confirm imports now work in this session.
  import_status <- .dip_python_import_status()
  if (!isTRUE(import_status$ok)) {
    message(
      "DIP installed Python packages, but import checks still fail in the current session. ",
      "Please restart R and retry, or install packages manually in the active Python environment.\n",
      "Underlying Python import error: ",
      import_status$error
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  # Avoid selecting/initializing Python at package load time.
  # Python dependency resolution is performed lazily in cDIP().
  invisible(NULL)
}
