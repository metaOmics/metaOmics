library(shiny)
dir <- "ui"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

shinyUI(
  navbarPage("metaOmics", id="nav",
    preproc_ui("preproc"),
    saved_data_ui("saved_data"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$script(src="js/message-handler.js")
    ),
    conditionalPanel("false", icon("crosshair"))
  )
)
