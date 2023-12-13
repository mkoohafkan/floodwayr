# {floodwayr}

An R package and Shiny app for evaluating floodway definitions.


## Installation

1. Install R. (https://cran.r-project.org/)
2. Install the `{remotes}` package.
```r
if (!require(remotes)) {
  install.packages("remotes")
}
```
3. Install the `{floodwayr}` package. This will also install all dependencies.
```r
remotes::install_github("mkoohafkan/floodwayr")
```


## Usage

The `{floodwayr}` package provides both a web app and a command line
interface.


### Web App

```r
library(floodwayr)

run_app()
```


### Command Line interface

```r
library(floodwayr)

demo_dir = file.path("test", "mckay_creek")

# evaluation lines
profiles = file.path(demo_dir, "Features",
  "Profile Lines Extracted.shp")
# model mesh
model_elements = file.path(demo_dir, "Features",
  "mesh.shp")

# base flood elevation
bfe = file.path(demo_dir, "2D_DS_100yr", 
    "WSE (01JAN3000 06 00 00).Terrain_wParkFill.Terrain_wParkFill.tif")
# floodway WSE
floodway_wse = file.path(demo_dir, "Floodway",
    "WSE (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif")
# depth x velocity raster (unit discharge)
floodway_dv = file.path(demo_dir, "Floodway",
    "D _ V (01JAN3000 05 00 00).Terrain_wParkFill.Terrain_wParkFill.tif")

results = run_cli(bfe, floodway_wse, floodway_dv,
  profiles, model_elements, map = TRUE)
```
