#' Raster Leaflet Map
#'
#' Create a leaflet map from a raster.
#'
#' @param raster A `SpatRaster` object.
#' @return A `leaflet` object.
#'
#' @importFrom terra plet nlyr crs
#' @importFrom leaflet addPolylines addPolygons hideGroup
#'   addLayersControl layersControlOptions
#' @keywords internal
map_raster = function(rastr = NULL, lines = NULL, mesh = NULL) {

  # TODO
  # leaflet and plet only work with web mercator... but reprojecting will change the results
  # if there is no way to make leaflet/plet use a different coordinate system, then produce 
  # static plots instead


  if (!is.null(rastr)) {
    mp = plet(rastr, seq_len(nlyr(rastr)),
      legend = "bottomleft", maxcell = 1e38)
  } else {
    mp = plet()
  }
  if (!is.null(lines)) {
    mp = mp |>
      addPolylines(data = lines, opacity = 1,
        color = values(lines)$color, group = "Evaluation Lines",
        label = values(lines)[["Name"]])
  }
  if (!is.null(mesh)) {
    mp = mp |>
      addPolygons(data = mesh, opacity = 1, group = "Model Mesh",
        color = "black", weight = 1, smoothFactor = 0, fill = FALSE) |>
      hideGroup("Model Mesh")
  }
  mp |>
    addLayersControl(baseGroups = names(rastr),
      overlayGroups = c("Evaluation Lines", "Model Mesh"),
      options = layersControlOptions(collapsed = FALSE))
}
