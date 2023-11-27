.onAttach = function(libname, pkgname) {
  if (is.null(getOption("shiny.maxRequestSize"))) {
    options(shiny.maxRequestSize = 1000 * 1024^2)
  }
}