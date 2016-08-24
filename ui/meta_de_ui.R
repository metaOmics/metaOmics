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
        h4("Response Type"),
        selectizeInput(ns("resp.type"), "", as.list(RESP.all)),
        uiOutput(ns("resp.type.option")),
        uiOutput(ns("toggle.option")),
        uiOutput(ns("advanced.option")),
	tags$hr(),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
      ),
      mainPanel(
        h3("Heat Map"),
        fluidRow(
          column(6, numericInput(ns("fdr.cut"), "FDR Cutoff", value=1e-9)),
          column(6, sliderInput(ns("scale"), "Image Size", value=1, min=0.5, max=4))
        ),
        h3(textOutput(ns("heatmap.info"))),
	imageOutput(ns('heatmap'), height="100%"),
        h3("Analysis Summary"),
        DT::dataTableOutput(ns("summary"))
      )
    )
  )
}
