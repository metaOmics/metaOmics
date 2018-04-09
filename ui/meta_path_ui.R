meta_path_ui <- function(id, label = "MetaPath") {
  ns <- NS(id)
  tabPanel("MetaPath", value=id,
    sidebarLayout(
      sidebarPanel(
        h3("MetaPath"),
        tags$hr(),
                   h4("Summary Table"),
                   br(),br(),
                   tableOutput(ns("summaryTable")),
                   tags$hr(),
                   bsCollapsePanel("About", "This MetaPath panel serves as an UI for MetaPath package. When there are multiple studies available on a related hypothesis, meta-analysis methods are necessary for joint pathway analysis. Two major approaches have been included in the MetaPath package to serve for this purpose: Comparative Pathway Integrator (CPI, default) and Meta-Analysis for Pathway Enrichment (MAPE).  Pathway
clustering with statistically valid text mining is included in the package to
reduce pathway redundancy to condense knowledge and increase interpretability
of clustering results.",
                                                a(strong("Tutorials"),     href="https://github.com/metaOmics/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                                                style = "primary"),     
        bsCollapsePanel("Glossary", strong("CDF"), " cumulative density function", br(),
                        strong("FDR"), " False Discovery Rate", 
                        style = "default"),
        
                    tags$hr(),
          radioButtons(ns("mixed"),"mixed data types?",c(No=F,Yes=T), F, inline=T),       
        bsCollapsePanel("Response Type", 
          selectizeInput(ns("resp.type"), "", as.list(RESP.all)),
          uiOutput(ns("resp.type.option")), style="primary"
        ),
        bsCollapsePanel("Individual Study Option",
                        uiOutput(ns("ind.method.opt")),style="primary"
        ),
          bsCollapsePanel("Advanced Options",
            tagList(
              uiOutput(ns("covariate.opt")),
              selectizeInput(ns("tail"), "Alternative Hypothesis", TAIL.all)
            ), style="info"
          ),        
	tags$hr(),
        selectizeInput(ns('pathway'), "Pathway Databases:", GENESET.all, multiple=T,
                       selected=c(GENESET.BioCarta, GENESET.GOBP, GENESET.GOCC,
                               GENESET.GOMF, GENESET.KEGG, GENESET.Reactome)),
        tags$hr(),
        bsCollapse(id="meta_de-advanced",
          bsCollapsePanel("Options",
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
        actionButton(ns('run'), 'Step 1: Run Pathway Analysis', 
		     icon=icon("rocket"), class="btn-success btn-run"),
        tags$hr(),
        bsCollapsePanel("Step 2: Pathway Clustering Diagnostics",
          uiOutput(ns('heatmap.opt')), style="primary"
        ),
        tags$hr(),
        bsCollapsePanel("Step 3: Clustering",
	  uiOutput(ns('clustering.opt')), style="primary"
        )
      ),
      mainPanel(
        h3("Analysis Summary"),
        DT::dataTableOutput(ns("summary")),
        tags$div(class="DocumentList",
          tags$ul(class="list-inline",
            tags$li(class="DocumentItem", imageOutput(ns('consensus'), height="50%")),
            tags$li(class="DocumentItem", imageOutput(ns('delta'), height="50%"))
          )
        ),
        tags$hr(),
        imageOutput(ns('heatmap'), height="50%") 
      )
    )
  )
}
