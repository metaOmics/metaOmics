setting_ui <- function(id, label = "global settings") {
  ns <- NS(id)
  tabPanel("Settings", value=id,
    mainPanel(
      h4("Directory for Saving Output Files:"),
      directoryInput(ns('directory'), label = 'select a directory')
    )
  )
}
