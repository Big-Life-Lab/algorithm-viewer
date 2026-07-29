# This file exists to allow shinylive::export() to locate and run the app.
# shinylive::export() requires an app.R in the package root directory.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
algorithm.viewer::run_app()
