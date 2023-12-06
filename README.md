# Floodwayr

Floodway Evaluation Tool.

## Demo

```r
library(floodwayr)

demo_dir = file.path("test", "mckay_creek")

# evaluation lines
profiles = load_shape_into_memory(file.path(demo_dir, "Features",
  "Profile Lines Extracted.shp"))
# model mesh
model_elements = load_shape_into_memory(file.path(demo_dir, "Features",
  "mesh.shp"))

# base flood elevation
bfe = load_raster_into_memory(file.path(demo_dir, "2D_DS_100yr", 
    "WSE (01JAN3000 06 00 00).Terrain_wParkFill.Terrain_wParkFill.tif"),
  clip = model_elements)
# floodway WSE
floodway_wse = load_raster_into_memory(file.path(demo_dir, "Floodway",
    "WSE (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif"),
  clip = model_elements)
# depth x velocity raster (unit discharge)
floodway_dv = load_raster_into_memory(file.path(demo_dir, "Floodway",
    "D _ V (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif"),
  clip = model_elements)

results = run_cli(bfe, floodway_wse, floodway_dv,
  profiles, model_elements, map = TRUE)
```