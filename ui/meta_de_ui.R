meta_de_ui <- function(id, label = "meta DE") {
  ns <- NS(id)
  tabPanel("Meta DE", value=id,
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("meta.type"), "Meta Method Type", as.list(META.TYPE.all)),
        uiOutput(ns("meta.type.opt")),
        bsCollapsePanel("Response Type",
          selectizeInput(ns("resp.type"), "", as.list(RESP.all)),
          uiOutput(ns("resp.type.option")), style="primary"
        ),
        uiOutput(ns("ind.method.opt")),
	tags$hr(),
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Advanced Options",
            tagList(
              radioButtons(ns("advanced.method"), "Use complete options",
                           c(Yes=T, No=F), F, inline=T),
              radioButtons(ns("parametric"), 'Parametric', inline=T,
                c(No=F, Yes=T), F),
              uiOutput(ns("para.opt")),
              uiOutput(ns("covariate.opt")),
              selectizeInput(ns("tail"), "Alternative Hypothesis", TAIL.all)
            ), style="info"
          )
        ),
	tags$hr(),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
      ),
      mainPanel(
        uiOutput(ns("plot.opt")),
        h3("Analysis Summary"),
        downloadButton(ns('downloadCsv'), 'Download Csv File'),
        DT::dataTableOutput(ns("summary"))
      )
    )
  )
}
