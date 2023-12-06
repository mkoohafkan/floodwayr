devtools::document()

extent = get_raster_extent("test/mckay_creek/2D_DS_100yr/terrain_extent.tif")
# base flood elevation
bfe = load_raster_into_memory("test/mckay_creek/2D_DS_100yr/WSE (01JAN3000 06 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# floodway WSE
floodway_wse = load_raster_into_memory("test/mckay_creek/Floodway/WSE (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# depth x velocity raster (unit discharge)
floodway_dv = load_raster_into_memory("test/mckay_creek/Floodway/D _ V (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# evaluation lines
profiles = load_shape_into_memory("test/mckay_creek/Features/Profile Lines Extracted.shp")

# model mesh
model_elements = load_shape_into_memory("test/mckay_creek/Features/mesh.shp")


surcharge = calculate_surcharge(bfe, floodway_wse, model_elements)
excess_counts = count_surcharge_exceedance(surcharge[["Class"]])

evaluation_lines = evaluate_profiles(bfe, floodway_wse, floodway_dv,
  profiles, model_elements)

res = map_results(surcharge, evaluation_lines, model_elements,
  bfe, floodway_wse, floodway_dv)