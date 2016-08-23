setting_ui <- function(id, label = "global settings") {
  ns <- NS(id)
  tabPanel("Settings", value=id,
    mainPanel(
      h4("Session Information"),
      verbatimTextOutput(ns("urlText")),
      h4("Directory for Saving Output Files:"),
      helpIcon("working_dir_help", HELP.working.dir),
      directoryInput(ns('directory'), label='select a directory')
    )
  )
}
