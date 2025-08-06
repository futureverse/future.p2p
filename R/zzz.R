.onLoad <- function(libname, pkgname) {
  import_future_functions()
  update_package_options()

  ## R CMD build
  register_vignette_engine_during_build_only(pkgname)
}

