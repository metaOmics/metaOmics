meta_qc_ui <- function(id, label = "MetaQC") {
    ns <- NS(id)

  tabPanel("MetaQC", value=id,
    sidebarLayout(
      sidebarPanel(
        h3("MetaQC"),
        tags$hr(),
#       uiOutput(ns("srcSelect")),
#       radioButtons(ns("useExample"), 'Use Example Dataset:', inline=T,c(Yes=T, No=F), T),
          h4("Summary Table"),
          br(),br(),
          tableOutput(ns("summaryTable")),
          tags$hr(),
          bsCollapsePanel("About", "This MetaQC panel serves as an UI for MetaQC package.
 MetaQC package provides an objective and quantitative tool to help determine the inclusion/exclusion of studies for meta-analysis. More specifically, MetaQC provides users with six quantitative quality control (QC) measures: including IQC, EQC, AQCg, CQCg, AQCp and CQCp.  In addition, visualization plots and summarization tables are generated using principal component analysis (PCA) biplots and standardized mean ranks (SMR) to assist in visualization and decision.",
                   a(strong("Tutorials"), href="https://github.com/metaOmics/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                               style = "primary"),
         tags$hr(),                
         bsCollapsePanel("Options",
          #radioButtons(ns("overlap.gene"), 'Take only overlapped genes: ', inline=T, c(Yes=T, No=F), T),
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
                     #radioButtons(ns("filter.pathway"), 'Perform pathway filtering: ', inline=T, c(Yes=T, No=F), T),
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
