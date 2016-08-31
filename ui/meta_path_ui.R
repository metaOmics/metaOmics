meta_path_ui <- function(id, label = "meta path") {
  ns <- NS(id)
  tabPanel("Meta Path", value=id,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('srcSelect')),
        uiOutput(ns('resp.opt')),
        selectizeInput(ns('pathway'), "Pathway Dayabases:", GENESET.all, multiple=T,
                       selected=c(GENESET.BioCarta, GENESET.GOBP, 
                               GENESET.GOMF, GENESET.KEGG, GENESET.Reactome)),
        tags$hr(),
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Advanced Options",
          tagList(
            uiOutput(ns('method.opt')),
            uiOutput(ns("mape.opt")),
            uiOutput(ns('stat.opt')),
            selectizeInput(ns('enrichment'), "Pathway Enrichment Method:", ENRICHMENT.all),
            uiOutput(ns('enrichment.opt')),
            uiOutput(ns('permute.opt')),
            numericInput(ns("size.min"), "pathway min gene size", 15),
            numericInput(ns("size.max"), "pathway max gene size", 500)
            ), style="info"
          )
        ),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
      ),
      mainPanel(
	uiOutput(ns('heatmap.opt')),
        tags$div(class="DocumentList",
          tags$ul(class="list-inline",
            tags$li(class="DocumentItem", imageOutput(ns('consensus'), height="100%")),
            tags$li(class="DocumentItem", imageOutput(ns('delta'), height="100%"))
          )
        ),
        h3("Analysis Summary"),
        DT::dataTableOutput(ns("summary"))
      )
    )
  )
}
