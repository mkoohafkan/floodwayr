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
#' @importFrom DT dataTableOutput
#' @keywords internal
ui = function() {
  dashboardPage(
    dashboardHeader(title = "Floodway Evaluation Tool"),
    dashboardSidebar(
      div(
        fileInput("bfe", "Base Flood Elevation", multiple = FALSE,
          accept = "image/*"),
        fileInput("floodwaywse", "Floodway WSE", multiple = FALSE,
          accept = "image/*"),
        fileInput("floodwaydv",
          "Floodway Depth x Velocity",
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
      hr(),
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
            dataTableOutput("evalresults"),
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
#'   withProgress incProgress outputOptions renderUI renderTable
#'   showNotification req stopApp
#' @importFrom shinydashboard infoBox
#' @importFrom leaflet renderLeaflet
#' @importFrom terra `coltab<-` classify project
#' @importFrom utf8 utf8_normalize
#' @importFrom DT renderDataTable datatable formatStyle styleEqual
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
  results_map = reactiveVal(NULL)

  observe({
    # load input data
    withProgress(message = "Processing Inputs...", value = 0, {
      tryCatch(
        eval_lines(load_shapefile(input$evaluation$datapath)),
        error = function(e) {
          showNotification(paste("Could not load evaluation lines.",
              "Did you upload all shapefile components?",
              "(.shp, .prj, .shx, .dbf)"),
            type = "error")
        }
      )
      tryCatch(
        model_mesh(load_shapefile(input$mesh$datapath)),
        error = function(e) {
          showNotification(paste("Could not load model mesh.",
              "Did you upload all shapefile components?",
              "(.shp, .prj, .shx, .dbf)"),
            type = "error")

        }
      )
      incProgress(0.25)
      tryCatch(
        bfe(load_raster_into_memory(input$bfe$datapath,
            req(model_mesh()))),
        error = function(e) {
          showNotification("Could not load BFE raster.",
            type = "error")
        }
      )
      incProgress(0.25)
      tryCatch(
        floodway_wse(load_raster_into_memory(input$floodwaywse$datapath,
            req(model_mesh()))),
        error = function(e) {
          showNotification("Could not load floodway WSE raster.",
            type = "error")
        }
      )
      incProgress(0.25)
      tryCatch(
        floodway_dv(load_raster_into_memory(input$floodwaydv$datapath,
            req(model_mesh()))),
        error = function(e) {
          showNotification("Could not load floodway depth x velocity raster.",
            type = "error")
        }
      )
      incProgress(0.25)
    })

    # calculate outputs
    withProgress(message = "Calculating...", value = 0, {
      # surcharge
      surcharge = calculate_surcharge(req(bfe()), req(floodway_wse()),
        req(model_mesh()))
      excess_counts = count_surcharge_exceedance(surcharge[["Class"]])

      incProgress(0.5)

      result_lines = evaluate_profiles(bfe(), floodway_wse(),
        floodway_dv(), eval_lines(), model_mesh())
      incProgress(0.5)
    })

    # mapping
    withProgress(message = "Rendering Results...", value = 0, {

      output$evalresults = renderDataTable({
        results_df = values(result_lines)[c("Name", "Average_Surcharge", "Class")]
        results_df["Average_Surcharge"] = number_label(results_df[["Average_Surcharge"]], 2)
        names(results_df) = c("Name", "Average Surcharge", "Class")
        datatable(results_df, options = list(
          columnDefs = list(list(visible = FALSE,
              targets = "Class")))) |>
          formatStyle("Average Surcharge", "Class",
            color = styleEqual(levels(results_df$Class), c("red", NA, "red")),
            fontWeight = styleEqual(levels(results_df$Class), c("bold", "normal", "bold")))
      })

      incProgress(0.2)

      output$surchargeabove = renderUI(
        infoBox("\U0394 BFE \U003E 1.5",
          paste(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]], "elements"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]] > 0,
            "remove", "ok"), lib = "glyphicon"),
          color = ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003E 1.5")]] > 0,
            "red", "aqua"),
          width = 12L)
      )
      output$surchargebelow = renderUI(
        infoBox("\U0394 BFE \U003C -0.5",
          paste(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]], "elements"),
          color = ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]] > 0,
            "red", "aqua"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("\U0394 BFE \U003C -0.5")]] > 0,
            "remove", "ok"), lib = "glyphicon"),
          width = 12L)
      )
      output$surchargenearbelow = renderUI(
        infoBox("-0.5 \U2264 \U0394 BFE \U003C 0",
          paste(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]], "elements"),
          icon = icon(ifelse(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]] > 0,
            "warning-sign" , "ok"), lib = "glyphicon"),
          color = ifelse(excess_counts[[utf8_normalize("-0.5 \U2264 \U0394 BFE \U003C 0")]] > 0,
            "yellow", "aqua"),
          width = 12L)
      )

      incProgress(0.1)

      results_map(map_results(surcharge, result_lines, model_mesh(),
          bfe(), floodway_wse(), floodway_dv()))

      incProgress(0.7)

    })
  }) |>
    bindEvent(input$compute)

  output$resultsmap = renderLeaflet({
    showNotification("Rendering map, this may take a while...",
      duration = NULL, type = "message")
    results_map()
  }) |>
    bindEvent(results_map(), ignoreNULL = TRUE, ignoreInit = TRUE)


  observe(stopApp()) |>
    bindEvent(input$quit)
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
