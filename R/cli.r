#' Run CLI
#'
#' Run the Floodway Evaluation command line interface.
#'
#' @inheritParams evaluate_profiles
#' @param map If `TRUE`, render an interactive map.
#' @return A named list of results.
#'
#' @importFrom shiny shinyApp shinyOptions
#' @export
run_cli = function(bfe, floodway_wse, floodway_dv,
  profiles, model_mesh, map = FALSE) {

  profiles = load_shape_into_memory(profiles)
  model_elements = load_shape_into_memory(model_elements)
  bfe = load_raster_into_memory(bfe, clip = model_elements)
  floodway_wse = load_raster_into_memory(floodway_wse, clip = model_elements)
  floodway_dv = load_raster_into_memory(floodway_dv, clip = model_elements)

  surcharge = calculate_surcharge(bfe, floodway_wse, model_elements)
  surcharge_exceedance = count_surcharge_exceedance(surcharge[["Class"]])

  evaluation_lines = evaluate_profiles(bfe, floodway_wse, floodway_dv,
    profiles, model_elements)

  if (map) {
    map = map_results(surcharge, evaluation_lines, model_elements,
      bfe, floodway_wse, floodway_dv)
    print(map)
  }

  list(
    evaluation_lines = evaluation_lines,
    surcharge = surcharge,
    surcharge_exceedance = surcharge_exceedance
  )
}
