meta_de_ui <- function(id, label = "meta DE") {
  ns <- NS(id)
  tabPanel("Meta DE", value=id,
    sidebarLayout(
      sidebarPanel(
        h4("Individual DE Method"),
        uiOutput(ns("ind.method")),
        h4("Meta Method"),
        selectizeInput(ns("meta.method"), "", as.list(META.all)),
        uiOutput(ns("meta.method.option")),
        uiOutput(ns("toggle.option")),
        uiOutput(ns("advanced.option"))
      ),
      mainPanel(
      )
    )
  )
}
