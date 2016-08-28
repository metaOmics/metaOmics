meta_path_ui <- function(id, label = "meta path") {
  ns <- NS(id)
  tabPanel("Meta Path", value=id,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('srcSelect')),
        selectizeInput(ns('pathway'), "Pathway Dayabases:", GENESET.all, multiple=T),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
      ),
      mainPanel(
      )
    )
  )
}
