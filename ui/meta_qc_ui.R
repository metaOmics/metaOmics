meta_qc_ui <- function(id, label = "Meta QC") {
    ns <- NS(id)

  tabPanel("Meta QC", value=id,
    sidebarLayout(
      sidebarPanel(
#       uiOutput(ns("srcSelect")),
       radioButtons(ns("useExample"), 'Use Example Dataset:', inline=T,c(Yes=T, No=F), T),
         bsCollapsePanel("Options",
          radioButtons(ns("overlap.gene"), 'Take only overlapped genes: ', inline=T, c(Yes=T, No=F), T),
            radioButtons(ns("filter.gene"), 'Perform gene filtering: ', inline=T, c(Yes=T, No=F), F),                     
               bsCollapsePanel(
                  tagList(
                    uiOutput(ns("filter.mean")),
                    uiOutput(ns("filter.var"))                 
                  ), style="info"),
                  tagList(
                    radioButtons(ns("adjust.pvalue.gene"), 'Use adjusted p-value for selecting DE genes', inline=T, c(Yes=T, No=F), T),                
                    numericInput(ns("pvalue.cut.gene"), "p-value cutoff for selecting DE genes", value=0.05),
               radioButtons(ns("adjust.pvalue.pathway"), 'Use adjusted p-value for selecting pathways', inline=T, c(Yes=T, No=F), T),                
                    numericInput(ns("pvalue.cut.pathway"), "p-value cutoff for selecting pathways", value=0.05)                 
                  ),                  
             style="primary"
           ),
        tags$hr(),                                       
          bsCollapsePanel("Advanced Options",
                tagList(
                     radioButtons(ns("filter.pathway"), 'Perform pathway filtering: ', inline=T, c(Yes=T, No=F), T),
                     uiOutput(ns("min")),
                     uiOutput(ns("max")),
                     numericInput(ns("permutation"),"Number of permutations", value=100)
            ), style="primary"
         ),
        actionButton(ns('run'), 'Run MetaQC Analysis', 
		     icon=icon("rocket"), class="btn-success btn-run")
       ),
       mainPanel(
           DT::dataTableOutput(ns("summary")),
           downloadButton(ns('downloadCsv'), 'Download Csv File'),         
           tags$hr(),
           imageOutput(ns('biplot'), height="100%")
      )
    )
  )
}
