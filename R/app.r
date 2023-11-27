#' Substitute Filename
substitute_filename = function(path, newname) {
  file.path(dirname(path), gsub("(.+)\\.(.+)$",
      sprintf("%s.\\2", newname), basename(path)))
}


#' @importFrom shiny fixedRow verticalLayout fileInput actionButton tabPanel
#'   column icon tabsetPanel tableOutput uiOutput
#' @importFrom shinydashboard dashboardPage dashboardHeader
#'   dashboardSidebar dashboardBody box
#' @importFrom leaflet leafletOutput
#' @keywords internal
ui = function() {
  dashboardPage(
    dashboardHeader(title = "Floodway Evaluation Tool"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      box(id = "inputbox",
        fixedRow(
          column(4L,
            fileInput("bfe", "Base Flood Elevation", multiple = FALSE,
              accept = "image/*"),
            fileInput("evaluation", "Evaluation Lines",
              multiple = TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx",
                ".shx", ".prj", ".cpg")),
          ),
          column(4L,
            fileInput("floodwaywse", "Floodway WSE", multiple = FALSE,
              accept = "image/*"),
            fileInput("floodwaydv",
              "Floodway Unit Discharge (Depth x Velocity)",
              multiple = FALSE, accept = "image/*")
          ),
          column(4L,
            fileInput("calcextent", "Analysis Extent (optional)",
              multiple = FALSE, accept = "image/*"),
            actionButton("compute", "Compute",
              style = 'margin-top:25px',
              icon = icon("flash", lib = "glyphicon"))
          )
        ),
        width = 12L
      ),
      box(id = "outputbox",
        column(9L,
          leafletOutput("resultsmap", height = 880)
        ),
        column(3L,
          verticalLayout(
            tableOutput("evalresults"),
            fixedRow(
              uiOutput("surchargeabove"),
              uiOutput("surchargebelow"),
            )
          )
        ),
        width = 12L,
        height = 900
      )
    )
  )
}





#' @importFrom shiny reactiveVal observe bindCache bindEvent
#' withProgress incProgress outputOptions renderUI
#' @importFrom shinydashboard valueBox
#' @importFrom leaflet renderLeaflet
#' @importFrom terra `coltab<-` classify project
server = function(input, output, session) {
  # inputs
  bfe = reactiveVal(NULL)
  floodway_wse = reactiveVal(NULL)
  floodway_dv = reactiveVal(NULL)
  eval_lines = reactiveVal(NULL)

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
      template_name = basename(tempfile())
      newnames = substitute_filename(input$evaluation$datapath,
        template_name)
      file.copy(input$evaluation$datapath, newnames)
      shp_name = newnames[grepl("\\.shp$", newnames)]
      eval_lines(
        load_shape_into_memory(shp_name)
      )
      incProgress(0.25)
    })

    # calculuate outputs
    withProgress(message = "Calculating...", value = 0, {
      # surcharge
      surcharge(
        calculate_raster_difference(floodway_wse(), bfe())
      )
      excess_surcharge(
        classify_surcharge(surcharge())
      )
      
      excess_count = sum(values(surcharge()) > 0, na.rm = TRUE)
      under_count = sum(values(excess_surcharge()) < 0, na.rm = TRUE)

      incProgress(0.25)
      # surcharge * unit discharge (numerator)
      dv_x_surcharge = calculate_raster_product(surcharge(),
        floodway_dv())
      incProgress(0.25)
      # extraction
      dv_x_surcharge_extract = extract_cells_by_shape(dv_x_surcharge,
        eval_lines(), id = "Name")
      floodway_dv_extract = extract_cells_by_shape(floodway_dv(),
        eval_lines(), id = "Name")
      incProgress(0.25)
      weighted_averages = compute_weighted_averages(dv_x_surcharge_extract,
        floodway_dv_extract, "Name")
      incProgress(0.25)
    })
    result_lines = isolate(eval_lines())
    result_lines$Value = round(weighted_averages$Value[match(weighted_averages$Name, result_lines$Name)], 2)
    result_lines$color = ifelse(result_lines$Value > 1.0 | result_lines$Value < 0.0, "red", "green")
    # mapping
    withProgress(message = "Mapping Results...", value = 0, {
      pr = c(
        project(bfe(), "EPSG:4326", threads = TRUE),
        project(floodway_wse(), "EPSG:4326", threads = TRUE),
        project(floodway_dv(), "EPSG:4326", threads = TRUE),
        project(surcharge(), "EPSG:4326", threads = TRUE),
        project(excess_surcharge(), "EPSG:4326", "near", threads = TRUE)
      )
      pr = c(bfe(), floodway_wse(), floodway_dv(), surcharge(),
        excess_surcharge())
      names(pr) = c("Base Flood Elevation", "Floodway WSE",
        "Floodway Unit Discharge", "Surcharge", "Surcharge Violation")
      pl = project(result_lines, "EPSG:4326")
      incProgress(0.5)
      output$resultsmap = renderLeaflet({
        map_raster(pr, pl)
      })
      incProgress(0.4)
      output$evalresults = renderTable({
        values(result_lines)[c("Name", "Value")]
      }, striped = TRUE, digits = 2)
      output$surchargeabove = renderUI(
        valueBox(excess_count, "# of cells with surcharge > 1.0")
      )
      output$surchargebelow = renderUI(
        valueBox(under_count, "# of cells with surcharge < 0.0")
      )
    })
  }) |>
    bindEvent(input$compute)

}

#' @importFrom shiny shinyApp shinyOptions
run_app = function() {
  shinyApp(ui(), server)
}
