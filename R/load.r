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
#' @param shape A file path to a vector.
#' @return A `SpatVector` object.
#'
#' @details If `shape` is already a `SpatVector` object,
#'   the function simply returns the object.
#'
#' @importFrom terra vect
#' @export
load_shape_into_memory = function(shape) {
  if (inherits(shape, "SpatVector")) {
    return(shape)
  } else {
    return(vect(shape))
  }
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
  ext(load_raster_into_memory(raster))
}
