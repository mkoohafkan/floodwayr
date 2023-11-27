devtools::load_all()

# compute extent
extent = get_raster_extent("test/mckay_creek/2D_DS_100yr/terrain_extent.tif")

# base flood elevation
bfe = load_raster_into_memory("test/mckay_creek/2D_DS_100yr/WSE (01JAN3000 06 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)

# floodway WSE
floodway_elev = load_raster_into_memory("test/mckay_creek/Floodway/WSE (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)

# depth x velocity raster (unit discharge)
floodway_dv = load_raster_into_memory("test/mckay_creek/Floodway/D _ V (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)

# evaluation lines
eval_lines = load_shape_into_memory("test/mckay_creek/Features/Profile Lines Extracted.shp") |>
  project(crs(bfe))


# compute surcharge
surcharge = calculate_raster_difference(floodway_elev, bfe)

excess_surcharge = classify_surcharge(surcharge)

# compute product of surcharge and unit discharge
dv_x_surcharge = calculate_raster_product(surcharge, floodway_dv)

# extract dv x surcharge along evaluation lines
dv_x_surcharge_extract = extract_cells_by_shape(dv_x_surcharge, eval_lines, id = "Name")
# extract surcharge along evaluation lines
floodway_dv_extract = extract_cells_by_shape(floodway_dv, eval_lines, id = "Name")

weighted_averages = compute_weighted_averages(dv_x_surcharge_extract, floodway_dv_extract, "Name")
weighted_averages["Value"] = round(weighted_averages[["Value"]], 2)

rr = c(bfe, floodway_elev, surcharge, excess_surcharge)
names(rr) = c("Base Flood Elevation", "Floodway WSE", "Surcharge", "Excess Surcharge")
map_raster(rr, terra:::project(eval_lines, "EPSG:4326"))


# on the fly terra projection not working?
rr = c(bfe, floodway_elev, surcharge, excess_surcharge)
map_raster(rr, terra:::project(eval_lines, "EPSG:4326"))



##################################

extent = get_raster_extent("test/mckay_creek/2D_DS_100yr/terrain_extent.tif")
# base flood elevation
bfe = load_raster_into_memory("test/mckay_creek/2D_DS_100yr/WSE (01JAN3000 06 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# floodway WSE
floodway_elev = load_raster_into_memory("test/mckay_creek/Floodway/WSE (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# depth x velocity raster (unit discharge)
floodway_dv = load_raster_into_memory("test/mckay_creek/Floodway/D _ V (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif",
  clip = extent)
# evaluation lines
eval_lines = load_shape_into_memory("test/mckay_creek/Features/Profile Lines Extracted.shp") |>
  project(crs(bfe))

excess_surcharge = classify_surcharge(bfe, floodway_elev)
excess_counts = count_surcharge_exceedance(excess_surcharge)
result_profiles = evaluate_profiles(bfe, floodway_elev, floodway_dv, eval_lines)
