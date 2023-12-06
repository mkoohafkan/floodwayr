#' Raster Difference
#'
#' Calculate the difference of two rasters.
#' 
#' @param raster1 A `SpatRaster` object.
#' @param raster2 A `SpatRaster` object.
#' @return The difference raster, i.e., `raster1 - raster2`.
#'
#' @keywords internal
calculate_raster_difference = function(raster1, raster2) {
  raster1 = load_raster_into_memory(raster1)
  raster2 = load_raster_into_memory(raster2)

  raster1 - raster2
}


#' Raster Product
#'
#' Calculate the product of two rasters.
#'
#' @inheritParams calculate_raster_difference
#' @return The product raster, i.e., `raster1 * raster2`.
#'
#' @keywords internal
calculate_raster_product = function(raster1, raster2) {
  raster1 = load_raster_into_memory(raster1)
  raster2 = load_raster_into_memory(raster2)

  raster1 * raster2
}
