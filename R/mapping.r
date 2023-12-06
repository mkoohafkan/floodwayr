#' Number Label
#'
#' Format numbers for map label display.
#'
#' @param x A numeric vector.
#' @param digits The number of digits to round and display.
#' @return A character vector.
#'
#' @keywords internal
number_label = function(x, digits = 0) {
  format(round(x, digits), nsmall = digits)
}


#' Map Results
#'
#' Generate a `leaflet` map of results.
#'
#' @param surcharge The surcharge `SpatVector`, i.e.,
#'   output of [calculate_surcharge()].
#' @param evaluation_lines The evaluation lines `SpatVector`, i.e.,
#'   output of [evaluate_profiles()].
#' @inheritParams evaluate_profiles
#' @return A `leaflet` object.
#'
#' @importFrom sf st_as_sf
#' @importFrom stats complete.cases
#' @importFrom terra extract project crs
#' @importFrom leaflet leaflet addPolygons addPolylines addLegend
#'   addProviderTiles hideGroup addLayersControl colorFactor colorNumeric
#'   addMapPane pathOptions layersControlOptions providerTileOptions
#' @export
map_results = function(surcharge, evaluation_lines, model_elements,
  bfe, floodway_wse, floodway_dv) {

  rasters = c(bfe, floodway_wse, floodway_dv)
  names(rasters) = c("Base_Flood_Elevation",
    "Floodway_Water_Surface_Elevation",
    "Floodway_Depth_x_Velocity")
  inputs = extract(rasters, model_elements, bind = TRUE,
    fun = "mean", na.rm = TRUE)

  # remove NA elements
  inputs = inputs[complete.cases(values(inputs)[names(rasters)])]
  sur = surcharge[!is.na(values(surcharge)["Surcharge"])]

  # project for mapping
  inputs = st_as_sf(project(inputs, crs("EPSG:4326")))
  sur = st_as_sf(project(sur, crs("EPSG:4326")))
  elem = st_as_sf(project(model_elements, crs("EPSG:4326")))
  eval_lines = st_as_sf(project(evaluation_lines, crs("EPSG:4326")))

  # color palettes
  wse_pal = colorNumeric("Blues",
    domain = range(c(inputs$Base_Flood_Elevation,
        inputs$Floodway_Water_Surface_Elevation),
      na.rm = TRUE), alpha = TRUE,
    na.color = "#00000000")
  dv_pal = colorNumeric("RdYlBu",
    domain = inputs$Floodway_Depth_x_Velocity, alpha = TRUE,
    na.color = "#00000000", reverse = TRUE)
  sur_pal = colorFactor(c("red", "yellow", "green", "red"),
    levels = levels(sur$Class), alpha = TRUE,
    na.color = "#00000000")
  eval_pal = colorFactor(c("red", "green", "red"),
    levels = levels(eval_lines$Class), alpha = TRUE,
    na.color = "#00000000")

  leaflet(inputs) |>
    addMapPane("inputs", zIndex = 420) |>
    addMapPane("mesh", zIndex = 440) |>
    addMapPane("lines", zIndex = 450) |>
    addProviderTiles("Esri.WorldImagery",
      options = providerTileOptions(minZoom = 4, maxNativeZoom = 18, maxZoom = 25)) |>
    addPolygons(group = "Base Flood Elevation", smoothFactor = 0,
      fillColor = ~wse_pal(Base_Flood_Elevation), fillOpacity = 1,
      label = ~number_label(Base_Flood_Elevation, 1),
      color = ~wse_pal(Base_Flood_Elevation), weight = 1, opacity = 1,
      options = pathOptions(pane = "inputs")) |>
    addLegend(group = "Base Flood Elevation",
      values = ~Base_Flood_Elevation, pal = wse_pal,
      position = "bottomleft", title = "Base Flood Elevation") |>
    addPolygons(group = "Floodway Water Surface Elevation", smoothFactor = 0,
      fillColor = ~wse_pal(Floodway_Water_Surface_Elevation), fillOpacity = 1,
      label = ~number_label(Floodway_Water_Surface_Elevation, 1),
      color = ~wse_pal(Floodway_Water_Surface_Elevation), weight = 1, opacity = 1,
      options = pathOptions(pane = "inputs")) |>
    addLegend(group = "Floodway Water Surface Elevation",
      values = inputs$Floodway_Water_Surface_Elevation, pal = wse_pal,
      position = "bottomleft", title = "Floodway Water Surface Elevation") |>
    addPolygons(group = "Floodway Depth x Velocity", smoothFactor = 0,
      fillColor = ~dv_pal(Floodway_Depth_x_Velocity),
      label = ~number_label(Floodway_Depth_x_Velocity, 1), fillOpacity = 1,
      color = ~dv_pal(Floodway_Depth_x_Velocity), weight = 1, opacity = 1,
      options = pathOptions(pane = "inputs")) |>
    addLegend(group = "Floodway Depth x Velocity", pal = dv_pal,
      values = inputs$Floodway_Depth_x_Velocity,
      position = "bottomleft", title = "Floodway Depth x Velocity") |>
    addPolygons(data = sur, group = "Surcharge", smoothFactor = 0,
      fillColor = ~sur_pal(Class), fillOpacity = 1,
      label = ~number_label(Surcharge, 1),
      color = ~sur_pal(Class), weight = 1, opacity = 1,
      options = pathOptions(pane = "inputs")) |>
    addLegend(group = "Surcharge",
      pal = sur_pal,
      values = levels(sur$Class),
      position = "bottomleft", title = "Surcharge") |>
    addPolygons(data = elem, group = "Model Elements",
      stroke = TRUE, weight = 1, color = "black",
      fill = FALSE, options = pathOptions(pane = "mesh")) |>
    addPolylines(data = eval_lines, group = "Evaluation Lines",
      color = ~eval_pal(Class), label = ~Name,
      opacity = 1, options = pathOptions(pane = "lines")) |>
    addLegend(group = "Evaluation Lines", pal = eval_pal,
      values = levels(eval_lines$Class),
      position = "bottomleft", title = "Evaluation Lines") |>
    addLayersControl(overlayGroups = c("Base Flood Elevation",
        "Floodway Water Surface Elevation",
        "Floodway Depth x Velocity", "Surcharge",
        "Evaluation Lines", "Model Elements"),
      options = layersControlOptions(collapsed = FALSE)) |>
    hideGroup(c("Base Flood Elevation",
        "Floodway Water Surface Elevation",
        "Floodway Depth x Velocity",
        "Model Elements"))


#    addLayersControl(baseGroups = c("Base Flood Elevation",
#        "Floodway Water Surface Elevation",
#        "Floodway Depth x Velocity", "Surcharge"),
#      overlayGroups = c("Evaluation Lines", "Model Elements"),
#      options = layersControlOptions(collapsed = FALSE))


}
