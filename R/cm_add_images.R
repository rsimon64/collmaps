
#' cm_add_images
#'
#' Provides an interface to examine georeferenced records.
#'
#' @import shinyFiles
#' @import shiny
#' @import miniUI
#' @examples if (interactive()) {
#'
#'    collmaps::cm_add_images()
#'
#' }
#' @export
cm_add_images <- function() {

  ui <- miniPage(
    gadgetTitleBar("Add Images"),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     fillRow(
                       fillCol(
                         shinyDirButton("directory",
                                        "Folder select",
                                        "Please select the image folder")
                         ,
                         textOutput("totalImages"),
                         checkboxInput("copyOriginals", "Copy Originals", TRUE),
                         selectInput("resizeWidth", "Resize to width",
                                     choices = c(100, 150, 200, 250, 300),
                                     selected = 200
                         )

                     ),
                     fillCol(
                       actionButton("startImport", "Start importing images")
                     )
                    )

                   )
      )
    )
  )

  server <- function(input, output, session) {
    volumes <- c(Home = fs::path_home(),
                  getVolumes()())
    shinyDirChoose(input, "directory", roots = volumes,
                   session = session,
                   restrictions = system.file(package = "base"))


    path <- reactive({parseDirPath(volumes, input$directory)})

    total_images <- reactive({
      length(list.files(path(), pattern = ".png|.jpg"))
    })

    output$totalImages <- renderText({
      HTML(paste("Total images selected:", total_images()))
    })

    observeEvent(input$startImport, {
      if (total_images() > 0) {
        withProgress({
          cm_import_images(path(),
                           scale_to_width = as.integer(input$resizeWidth),
                           include_original = as.logical(input$copyOriginals))
        })
      }
      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })

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
