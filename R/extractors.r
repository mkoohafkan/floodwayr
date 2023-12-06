#' Extract Raster Cells
#'
#' Extract raster cells along a line or shape.
#'
#' @inheritParams load_raster_into_memory
#' @inheritParams load_shape_into_memory
#' @param method The extraction method to use. For
#'   `method = "average"`, the average of raster values overlapping
#'   each shape is extracted. For `method = "centroid"`, the raster
#'   value occurring at the shape centroids is extracted.
#' @return A subset of `raster` cells intersecting with `shapes`.
#'
#' @importFrom terra extract values centroids
#' @keywords internal
extract_cells_by_shape = function(raster, shape,
  method = c("average", "centroid")) {
  raster = load_raster_into_memory(raster)
  shape = load_shape_into_memory(shape)

  method = match.arg(method)
  if (method == "average") {
    result = extract(raster, shape, bind = TRUE,
      fun = "mean", na.rm = TRUE)
  } else {
    ctrds = extract(raster, centroids(shape), bind = FALSE)
    result = shape
    values(result) = cbind(values(shape), ctrds[names(ctrds)[-1]])
  }
  names(result)[names(result) == names(raster)] = "Value"
  result
}


#' Drop NA Elements
#'
#' Drop elements with NA values for the specified field.
#'
#' @inheritParams load_shape_into_memory
#' @param field The field name to search for NA values.
#' @return The `SpatVector` object.
#'
#' @importFrom terra values
#' @keywords internal
drop_na_elements = function(shape, field) {
  shape[!is.na(values(shape)[[field]])]
}
