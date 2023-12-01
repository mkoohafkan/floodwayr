#' Evaluate Profiles
#'
#' Evaluate profiles according to FEMA methodology.
#'
#' @param bfe Base Flood Elevation `SpatRaster`.
#' @param floodway_wse Floodway water surface elevation `SpatRaster`.
#' @param floodway_dv Floodway depth-velocity `SpatRaster`.
#' @param profiles Evaluation profiles `SpatVector`.
#' @param model_elements Model elements `SpatVector`.
#' @return A dataframe.
#'
#' @importFrom terra values `values<-` intersect aggregate
#' @export
evaluate_profiles = function(bfe, floodway_wse, floodway_dv, profiles,
  model_elements) {

  profiles = load_shape_into_memory(profiles)

  # compute surcharge
  surcharge = calculate_surcharge(bfe, floodway_wse, model_elements)
  unit_discharge = calculate_unit_discharge(floodway_dv, model_elements)

  result = intersect(intersect(profiles, surcharge["Surcharge"]),
    unit_discharge["Unit_Discharge"])
  values(result)["Numerator"] = values(result)[["Surcharge"]] *
    values(result)[["Unit_Discharge"]]
  result = aggregate(result, by = "FID", dissolve = TRUE, fun = "sum",
    na.rm = TRUE)
  values(result)["Average_Surcharge"] = values(result)[["sum_Numerator"]] /
    values(result)[["sum_Unit_Discharge"]]

  keep_nms = c(intersect(names(result), names(profiles)), "sum_Surcharge",
    "sum_Unit_Discharge", "Average_Surcharge")
  values(result) = values(result)[keep_nms]

  cut_values = c(-Inf, 0, 1, Inf)
  cut_labels = utf8_normalize(c("\U0394 BFE \U003C 0",
      "0 \U2264 \U0394 BFE \U003C 1",
      "\U0394 BFE \U003E 1"))
  cut_colors = c("red", "green", "red")

  values(result)["Class"] = cut(round(values(result)[["Average_Surcharge"]], 2),
    cut_values, cut_labels)
  values(result)["color"] = cut(round(values(result)[["Average_Surcharge"]], 2),
    cut_values, cut_colors)

  result
}


#' Calculate Surcharge
#'
#' Calculate and classify surcharge according to FEMA guidance.
#'
#' @inheritParams evaluate_profiles
#' @return A classified `SpatRaster` object.
#'
#' @importFrom terra values `values<-`
#' @importFrom utf8 utf8_normalize
#' @export
calculate_surcharge = function(bfe, floodway_wse, model_elements) {

  surcharge = calculate_raster_difference(floodway_wse, bfe)
  cells = extract_cells_by_shape(surcharge, model_elements, method = "centroid")
  names(cells) = replace(names(cells), names(cells) == "Value", "Surcharge")

  cut_values = c(-Inf, -0.51, -0.01, 1.5, Inf)
  cut_labels = utf8_normalize(c("\U0394 BFE \U003C -0.5",
      "-0.5 \U2264 \U0394 BFE \U003C 0",
      "0 \U2264 \U0394 BFE \U003C 1.5",
      "\U0394 BFE \U003E 1.5"))
  cut_colors = c("red", "yellow", "green", "red")

  values(cells)["Class"] = cut(round(values(cells)[["Surcharge"]], 1),
    cut_values, cut_labels)
  values(cells)["color"] = cut(round(values(cells)[["Surcharge"]], 1),
    cut_values, cut_colors)
  cells
}


#' Calculate Unit Discharge
#'
#' Calculate unit discharge according to FEMA guidance.
#' Essentially, this function averages the floodway depth x velocity
#' raster across model elements.
#'
#' @inheritParams evaluate_profiles
calculate_unit_discharge = function(floodway_dv, model_elements) {

  floodway_dv = load_raster_into_memory(floodway_dv)
  model_elements = load_shape_into_memory(model_elements)

  cells = extract_cells_by_shape(floodway_dv, model_elements, "average")
  names(cells) = replace(names(cells), names(cells) == "Value",
    "Unit_Discharge")
  cells
}


#' Count Cells By Surcharge Exceedance
#'
#' Count the number of model elements that exceed FEMA guidance on
#' allowable surcharge.
#'
#' @param classes A vector of surcharge classes.
#' @return A named vector.
#'
#' @importFrom utf8 utf8_normalize
#' @export
count_surcharge_exceedance = function(classes) {
  keep_nms = utf8_normalize(c("\U0394 BFE \U003C -0.5",
      "-0.5 \U2264 \U0394 BFE \U003C 0",
      "\U0394 BFE \U003E 1.5"))
  fq = table(classes)
  fq[setdiff(keep_nms, names(fq))] = 0
  fq[keep_nms]
}
