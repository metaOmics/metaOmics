meta_de_ui <- function(id, label = "meta DE") {
  ns <- NS(id)
  tabPanel("Meta DE", value=id,
    sidebarLayout(
      sidebarPanel(
        bsCollapse(id="meta_de-opts", multiple=TRUE,
          bsCollapsePanel("Setting Individual Study Method",
            uiOutput(ns("ind.method")), style="primary"
          ),
          bsCollapsePanel("Meta Method",
            selectizeInput(ns("meta.method"), "", as.list(META.all)),
            uiOutput(ns("meta.method.option")), style="primary"
          ),
          bsCollapsePanel("Response Type",
            selectizeInput(ns("resp.type"), "", as.list(RESP.all)),
            uiOutput(ns("resp.type.option")), style="primary"
          )
        ),
	tags$hr(),
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Advanced Options",
            tagList(
              radioButtons(ns("asymptotic.p"), 'Asymptotic P', inline=T,
                c(No=F, Yes=T), F
              ),
              uiOutput(ns("asym.option")),
              selectizeInput(ns("tail"), "Alternative Hypothesis", TAIL.all)
            ), style="info"
          )
        ),
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
        downloadButton(ns('downloadCsv'), 'Download Csv File'),
        DT::dataTableOutput(ns("summary"))
      )
    )
  )
}
