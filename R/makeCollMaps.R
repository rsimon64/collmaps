
#' makeCollMaps
#'
#' @return
#' @export
makeCollMaps <- function() {

  ui <- miniPage(
    gadgetTitleBar("Collection Maps"),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
         miniContentPanel(
           fillRow(
             fillCol(
               fileInput('database', 'Choose file to upload',
                         accept = c(
                           '.xlsx',
                           '.xls'
                         )
               ),
               uiOutput('idField'),
               uiOutput('latField'),
               uiOutput('lonField')
             ),
             fillCol(
               uiOutput('displayFields'),
               selectInput("maptype", "Maptype",
                           providers[stringr::str_starts(providers, "Stamen|Esri")],
                           selected = "Stamen.TonerLite")
             )
           )


         )
      ),
      miniTabPanel("Map", icon = icon("map-o"),
        miniContentPanel(
          leaflet::leafletOutput("mymap", height = "100%")
        )
      ),
      miniTabPanel("Data", icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table")
                   )
      )
    )
  )

  server <- function(input, output, session) {

    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)

    db <- reactive({
      inFile <- input$database

      if (is.null(inFile))
        return(NULL)
      readxl::read_excel(inFile$datapath)
    })

    fieldNames <- reactive({
      names(db())
    })

    output$idField <- renderUI({
      selectInput("id", "ID",  choices = fieldNames(), selected = fieldNames()[1])
    })

    output$latField <- renderUI({
      latIdx <- which(stringr::str_detect(tolower(fieldNames()), "lat"))
      selectInput("lat", "Longitude",  choices = fieldNames(), selected = fieldNames()[latIdx])
    })

    output$lonField <- renderUI({
      lonIdx <- which(stringr::str_detect(tolower(fieldNames()), "lon"))
      selectInput("lon", "Lonfitude",  choices = fieldNames(), selected = fieldNames()[lonIdx])
    })

    output$displayFields <- renderUI({
      varSelectInput("display", "Column to use in display",  db(),
                     selected = "ID",
                     multiple = TRUE)
    })



    output$mymap <- renderLeaflet({
      # message(str(input$display))
      fields <- as.character(input$display)
      labels <- paste0("ID: ",
                       db()$ID, "<br/>")

      n <- length(fields)
      if (n > 1) {
        for (i in 2:n) {
          labels2 <- paste0(fields[i], ": ",
                           db()[[fields[i]]], "<br/>")
          labels <- paste(labels, labels2)
        }
      }

      leaflet() %>%
        addProviderTiles( input$maptype, # providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = db()[, c(input$lon, input$lat)],
                   clusterOptions = markerClusterOptions(),
                   popup = labels
                   )
    })


    output$table <- DT::renderDataTable({
      db()}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
    )

    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)

}
