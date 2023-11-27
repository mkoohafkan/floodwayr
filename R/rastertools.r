#' Extract Raster Cells
#'
#' Extract raster cells along a line or shape.
#'
#' @param raster A `SpatRaster` object.
#' @param shapes A `SpatVector` object.
#' @param id The attribute of `shapes` to use as the ID in the output.
#' @return A subset of `raster` cells intersecting with `shapes`.
#'
#' @importFrom terra extract values
#' @keywords internal
extract_cells_by_shape = function(raster, shapes, id = NULL) {
  shape = load_shape_into_memory(shapes)
  raster = load_raster_into_memory(raster)

  result = extract(raster, shapes)
  names(result)[names(result) == names(raster)] = "Value"

  if (!is.null(id)) {
    result[id] = values(shapes)[result[["ID"]], id]
  }
  result
}


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


#' Evaluate Profiles
#'
#' Evaluate profiles according to FEMA methodology.
#'
#' @param bfe Base Flood Elevation `SpatRaster`.
#' @param floodway_wse Floodway water surface elevation `SpatRaster`.
#' @param floodway_dv Floodway depth-velocity `SpatRaster`.
#' @param profiles Evaluation profiles `SpatVector`
#' @return A dataframe.
#'
#' @importFrom terra project crs values `values<-`
#' @export
evaluate_profiles = function(bfe, floodway_wse, floodway_dv, profiles,
  id = "Name") {

  bfe = load_raster_into_memory(bfe)
  floodway_wse = load_raster_into_memory(floodway_wse)
  floodway_dv = load_raster_into_memory(floodway_dv)
  profiles = load_shape_into_memory(profiles) |>
    project(crs(bfe))

  # compute surcharge
  surcharge = calculate_raster_difference(floodway_wse, bfe)
  # compute product of surcharge and unit discharge
  dv_x_surcharge = calculate_raster_product(surcharge, floodway_dv)

  # extract dv x surcharge along evaluation lines
  dv_x_surcharge_extract = extract_cells_by_shape(dv_x_surcharge,
    profiles, id = id)
  # extract surcharge along evaluation lines
  floodway_dv_extract = extract_cells_by_shape(floodway_dv, profiles,
    id = id)

  # compute evaluation lines
  weighted_averages = compute_weighted_averages(dv_x_surcharge_extract,
    floodway_dv_extract, id)

  result_lines = profiles
  values(result_lines) = merge(values(result_lines), weighted_averages,
    on = id)
  result_lines$Value = round(result_lines$Value, 2)
  result_lines$color = ifelse(
    result_lines$Value > 1.0 | result_lines$Value < 0.0,
    "red", "green"
  )
  result_lines
}


#' Classify Surcharge
#'
#' Classify surcharge according to FEMA guidance.
#'
#' @inheritParams extract_cells_by_shape
#' @return A classified `SpatRaster` object.
#'
#' @importFrom terra classify `coltab<-`
#' @importFrom utf8 utf8_normalize
#' @export
classify_surcharge = function(bfe, floodway_wse) {

  bfe = load_raster_into_memory(bfe)
  floodway_wse = load_raster_into_memory(floodway_wse)
  surcharge = calculate_raster_difference(floodway_wse, bfe)

  craster = classify(round(surcharge, 1), c(-Inf, -1, 0, 1, Inf))
  levels(craster) = data.frame(ID = 0:3,
    Value = utf8_normalize(c("\U0394 BFE \U003C -1",
        "-1 \U2264 \U0394 BFE \U003C 0",
        "0 \U2264 \U0394 BFE \U003C 1",
        "\U0394 BFE \U003E 1"))
  )
  coltab(craster) = data.frame(value = seq(0L, 3L),
    color = c("red", "yellow", "green", "red"))
  craster
}







#' Count Cells By Surcharge Exceedance
#'
#' Count the number of raster cells that exceed FEMA guidance.
#'
#' @inheritParams classify_surcharge
#' @return A named vector.
#'
#' @importFrom terra freq
#' @importFrom utf8 utf8_normalize
#' @export
count_surcharge_exceedance = function(raster) {
  keep_nms = utf8_normalize(c("\U0394 BFE \U003C -1",
      "-1 \U2264 \U0394 BFE \U003C 0",
      "\U0394 BFE \U003E 1"))
  fq = freq(raster, bylayer = FALSE)
  res = fq$count
  # terra is doing something here, need to renormalize utf8
  names(res) = utf8_normalize(fq$value)
  res[setdiff(keep_nms, names(res))] = 0
  res[keep_nms]
}
