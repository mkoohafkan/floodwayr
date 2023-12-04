#' Load Raster
#'
#' Load a raster from a file.
#'
#' @param raster A file path to a raster.
#' @param clip A `SpatRaster` or `SpatExtent` object to crop `raster` to.
#' @return A `SpatRaster` object.
#'
#' @importFrom terra rast crop
#' @export
load_raster_into_memory = function(raster, clip = NULL) {
  if (!inherits(raster, "SpatRaster")) {
    raster = rast(raster)
    names(raster) = make.names(names(raster))
  }
  if (!is.null(clip)) {
    raster = crop(raster, get_raster_extent(clip))
  }
  raster
}


#' Load Shape
#'
#' Load a vector shape from a file.
#'
#' @param shape A `SpatVector` object or file path to a vector layer.
#' @return A `SpatVector` object.
#'
#' @details If `shape` is already a `SpatVector` object,
#'   the function simply returns the object.
#'
#' @importFrom terra vect
#' @export
load_shape_into_memory = function(shape) {
  if (!inherits(shape, "SpatVector")) {
    shape = vect(shape)
    names(shape) = make.names(names(shape))
  }
  return(shape)
}


#' Get Raster Extent
#'
#' Get the extent from a `SpatRaster` object.
#'
#' @inheritParams load_raster_into_memory
#' @return A `SpatExtent` object.
#'
#' @importFrom terra ext
#' @export
get_raster_extent = function(raster) {
  if (!inherits(raster, c("SpatRaster", "SpatVector"))) {
    raster = load_raster_into_memory(raster)
  }
  ext(raster)
}
