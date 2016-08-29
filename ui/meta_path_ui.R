meta_path_ui <- function(id, label = "meta path") {
  ns <- NS(id)
  tabPanel("Meta Path", value=id,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('srcSelect')),
        selectizeInput(ns('resp.type'), "Response Type:", RESP.all),
        selectizeInput(ns('pathway'), "Pathway Dayabases:", GENESET.all, multiple=T),
        numericInput(ns("q_cutoff"), "FDR cut off", NULL),
        numericInput(ns("Num_Clusters"), "number of clusters", NULL),
        tags$hr(),
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Advanced Options",
          tagList(
            selectizeInput(ns('method'), "Software:", DE.METHOD.all),
            uiOutput(ns('method.opt')),
            uiOutput(ns('stat.opt')),
            selectizeInput(ns('enrichment'), "Pathway Enrichment Method:", ENRICHMENT.all),
            uiOutput(ns('enrichment.opt')),
            uiOutput(ns('permute.opt')),
            numericInput(ns("size.min"), "pathway min gene size", NULL),
            numericInput(ns("size.max"), "pathway max gene size", NULL)
            ), style="info"
          )
        ),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
      ),
      mainPanel(
      )
    )
  )
}
