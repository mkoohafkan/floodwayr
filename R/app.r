#' Substitute Filename
#'
#' Substitute filenames for loading a multi-file object such as shapefiles.
#'
#' @param paths A vector of filepaths. Each entry must have a
#'   differentfile  extension.
#' @param name_template The filename (without extension) to use as
#'   the new base name for each file.
#' @return The updated vector of file paths.
#'
#' @keywords internal
substitute_filename = function(paths, name_template) {
  file.path(dirname(paths), gsub("(.+)\\.(.+)$",
      sprintf("%s.\\2", name_template), basename(paths)))
}


#' Load Shapefile
#'
#' Load a shapefile from a set of (misnamed) files.
#'
#' @inheritParams substitute_filename
#' @return A `SpatVector` object.
#'
#' @keywords internal
load_shapefile = function(paths) {
  template = basename(tempfile())
  new_paths = substitute_filename(paths, template)
  if (any(duplicated(basename(new_paths)))) {
    stop("Duplicate file extensions supplied: ",
      paste(names(which(table(new_paths) > 1L)), collapse = ", "))
  }
  if (!all(file.copy(paths, new_paths))) {
    stop("Failed to rename shapefile components")
  }
  shp_name = new_paths[grepl("\\.shp$", new_paths)]
  load_shape_into_memory(shp_name)
}

#' @importFrom shiny fixedRow verticalLayout fileInput actionButton tabPanel
#'   column icon tabsetPanel tableOutput uiOutput div hr
#' @importFrom shinydashboard dashboardPage dashboardHeader
#'   dashboardSidebar dashboardBody box
#' @importFrom leaflet leafletOutput
#' @keywords internal
ui = function() {
  dashboardPage(
    dashboardHeader(title = "Floodway Evaluation Tool"),
    dashboardSidebar(
      div(
        fileInput("bfe", "Base Flood Elevation", multiple = FALSE,
          accept = "image/*"),
        fileInput("calcextent", "Analysis Extent (optional)",
          multiple = FALSE, accept = "image/*")
      ),
      hr(),
      div(
        fileInput("floodwaywse", "Floodway WSE", multiple = FALSE,
          accept = "image/*"),
        fileInput("floodwaydv",
          "Floodway Unit Discharge (Depth x Velocity)",
          multiple = FALSE, accept = "image/*")
      ),
      hr(),
      div(
        fileInput("evaluation", "Evaluation Lines",
          multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx",
            ".shx", ".prj", ".cpg")),
        fileInput("mesh", "Model Mesh",
          multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx",
            ".shx", ".prj", ".cpg"))
      ),
      div(
        column(6L,
          actionButton("compute", "Compute",
            icon = icon("flash", lib = "glyphicon"))
        ),
        column(6L,
          actionButton("quit", "Quit",
            icon = icon("off", lib = "glyphicon"))
        )
      )
    ),
    dashboardBody(
      box(id = "outputbox",
        column(9L,
          leafletOutput("resultsmap", height = "85vh")
        ),
        column(3L,
          verticalLayout(
            tableOutput("evalresults"),
            uiOutput("surchargeabove"),
            uiOutput("surchargebelow"),
            uiOutput("surchargenearbelow")
          )
        ),
        width = 12L,
        height = "90vh"
      )
    )
  )
}





#' @importFrom shiny reactiveVal observe bindCache bindEvent
#' withProgress incProgress outputOptions renderUI renderTable
#' @importFrom shinydashboard infoBox
#' @importFrom leaflet renderLeaflet
#' @importFrom terra `coltab<-` classify project
#' @importFrom utf8 utf8_normalize
server = function(input, output, session) {
  # inputs
  bfe = reactiveVal(NULL)
  floodway_wse = reactiveVal(NULL)
  floodway_dv = reactiveVal(NULL)
  eval_lines = reactiveVal(NULL)
  model_mesh = reactiveVal(NULL)

  # outputs
  surcharge = reactiveVal(NULL)
  excess_surcharge = reactiveVal(NULL)
  floodway_dv_extract = reactiveVal(NULL)
  eval_results = reactiveVal(NULL)

  observe({
    # load input data
    withProgress(message = "Processing Inputs...", value = 0, {
      bfe(
        load_raster_into_memory(input$bfe$datapath,
          input$calcextent$datapath)
      )
      incProgress(0.25)
      floodway_wse(
        load_raster_into_memory(input$floodwaywse$datapath,
          input$calcextent$datapath)
      )
      incProgress(0.25)
      floodway_dv(
        load_raster_into_memory(input$floodwaydv$datapath,
          input$calcextent$datapath)
      )
      incProgress(0.25)
      eval_lines(load_shapefile(input$evaluation$datapath))
      model_mesh(load_shapefile(input$mesh$datapath))
      incProgress(0.25)
    })

    # calculuate outputs
    withProgress(message = "Calculating...", value = 0, {
      # surcharge
      surcharge = calculate_raster_difference(floodway_wse(), bfe())
      excess_surcharge = classify_surcharge(bfe(), floodway_wse())
      excess_counts = count_surcharge_exceedance(excess_surcharge)
      incProgress(0.25)

      result_lines = evaluate_profiles(bfe(), floodway_wse(),
        floodway_dv(), eval_lines())
      incProgress(0.75)
    })
    # mapping
    withProgress(message = "Mapping Results...", value = 0, {
      pr = c(
        project(bfe(), "EPSG:4326", threads = TRUE),
        project(floodway_wse(), "EPSG:4326", threads = TRUE),
        project(floodway_dv(), "EPSG:4326", threads = TRUE),
        project(surcharge, "EPSG:4326", threads = TRUE),
        project(excess_surcharge, "EPSG:4326", "near", threads = TRUE)
      )
      pr = c(bfe(), floodway_wse(), floodway_dv(), surcharge,
        excess_surcharge)
      names(pr) = c("Base Flood Elevation", "Floodway WSE",
        "Floodway Unit Discharge", "Surcharge", "Surcharge Violation")
      pl = project(result_lines, "EPSG:4326")
      pm = project(model_mesh(), "EPSG:4326")
      incProgress(0.5)
      output$resultsmap = renderLeaflet({
        map_raster(pr, pl, pm)
      })
      incProgress(0.4)
      output$evalresults = renderTable({
        values(result_lines)[c("Name", "Value")]
      }, striped = TRUE, digits = 2)
      output$surchargeabove = renderUI(
        infoBox("\U0394 BFE \U003E 1.5",
          paste(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]], "cells"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]] > 0,
            "remove", "ok"), lib = "glyphicon"),
          color = ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]] > 0,
            "red", "aqua"),
          width = 12L)
      )
      output$surchargebelow = renderUI(
        infoBox("\U0394 BFE \U003C -0.5",
          paste(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]], "cells"),
          color = ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]] > 0,
            "red", "aqua"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]] > 0,
            "remove", "ok"), lib = "glyphicon"),
          width = 12L)
      )
      output$surchargenearbelow = renderUI(
        infoBox("-0.5 \U2264 \U0394 BFE \U003C 0",
          paste(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]], "cells"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]] > 0,
            "warning-sign" , "ok"), lib = "glyphicon"),
          color = ifelse(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]] > 0,
            "yellow", "aqua"),
          width = 12L)
      )
    })
  }) |>
    bindEvent(input$compute)

}

#' Run App
#'
#' Run The Floodway Evaluation app
#'
#' @importFrom shiny shinyApp shinyOptions
#' @export
run_app = function() {
  shinyApp(ui(), server)
}
