#' Raster Leaflet Map
#'
#' Create a leaflet map from a raster.
#'
#' @param raster A `SpatRaster` object.
#' @return A `leaflet` object.
#'
#' @importFrom terra plet nlyr crs
#' @importFrom leaflet addPolylines addLayersControl layersControlOptions
#' @keywords internal
map_raster = function(rastr = NULL, shp = NULL) {

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
  if (!is.null(shp)) {
    mp = mp |>
      addPolylines(data = shp, opacity = 1,
        color = values(shp)$color, group = "Evaluation Lines",
        label = values(shp)[["Name"]]) |>
      addLayersControl(baseGroups = names(rastr),
        overlayGroups = c("Evaluation Lines"),
        options = layersControlOptions(collapsed = FALSE))
  }
  mp
}
