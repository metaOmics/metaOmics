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
                c(No=F, Yes=T), T),
              uiOutput(ns("para.opt")),
              uiOutput(ns("covariate.opt")),
              selectizeInput(ns("tail"), "Alternative Hypothesis", TAIL.all)
            ), style="info"
          )
        ),
	tags$hr(),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run") , 

        tags$hr(),                                       
        selectizeInput(ns('pathway'), "Pathway Dayabases:", GENESET.all, multiple=T,
                       selected=c(GENESET.BioCarta, GENESET.GOBP, GENESET.GOCC,
                               GENESET.GOMF, GENESET.KEGG, GENESET.Reactome)),
        tags$hr(),                               
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Pathway Analysis Options",
          tagList(
            selectizeInput(ns('enrichment'), "Pathway Enrichment Method:", ENRICHMENT.all),
            uiOutput(ns('enrichment.opt')),
            numericInput(ns("size.min"), "pathway min gene size", 15),
            numericInput(ns("size.max"), "pathway max gene size", 500)
            ), style="info"
          )
        ),
        tags$hr(),                                       
        actionButton(ns('runpath'), 'Run Pathway Analysis', 
		     icon=icon("rocket"), class="btn-success btn-run")                    
      ),
      mainPanel(
        uiOutput(ns("plot.opt")),
        h3("Analysis Summary"),
        downloadButton(ns('downloadCsv'), 'Download Csv File'),
        DT::dataTableOutput(ns("summary")),
        downloadButton(ns('downloadPathwayCsv'), 'Download Csv File of Pathway Result'),  
        DT::dataTableOutput(ns("pathresult"))                       
      )
    )
  )
}
