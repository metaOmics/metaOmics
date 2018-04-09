meta_de_ui <- function(id, label = "MetaDE") {
  ns <- NS(id)
  tabPanel("MetaDE", value=id,
    sidebarLayout(
      sidebarPanel(
        h3("MetaDE"),
        tags$hr(),
          h4("Summary Table"),
          br(),br(),
          tableOutput(ns("summaryTable")),
          tags$hr(),
          bsCollapsePanel("About", "This MetaDE panel serves as an UI for MetaDE package.
 MetaDE package implements 12 major meta-analysis methods for differential expression analysis falling into 3 main categories: combining p-values, combining effect sizes and others. It allows  the input of either microarray (continuous intensity) or RNA-seq data (count) for individual study analysis. In addition, it also incorporates a downstream pathway analysis for functional annotation of the identified DE genes.",
                   a(strong("Tutorials"), href="https://github.com/metaOmics/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                               style = "primary"),
        bsCollapsePanel("Glossary", strong("AW Fisher"), " adaptively-weighted Fisher", br(),
                        strong("DE analysis"), " differential expression analysis", br(),
                        strong("FDR"), " False Discovery Rate", br(),
                        strong("rOP"), " rth ordered p-value",
                        style = "default"),
        
         tags$hr(),      
        selectizeInput(ns("meta.type"), "Meta Method Type", as.list(META.TYPE.all)),
        uiOutput(ns("meta.type.opt")),
        radioButtons(ns("mixed"),"mixed data types?",c(No=F,Yes=T), F, inline=T),
        bsCollapsePanel("Response Type",
          selectizeInput(ns("resp.type"), "", as.list(RESP.all)),
          uiOutput(ns("resp.type.option")), style="primary"
        ),
        bsCollapsePanel("Individual Study Option",
          uiOutput(ns("ind.method.opt")),style="primary"
        ),
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
        selectizeInput(ns('pathway'), "Pathway Databases:", GENESET.all, multiple=T,
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
