meta_pca_ui <- function(id, label = "MetaPCA") {
    ns <- NS(id)
    tabPanel("MetaPCA", value=id,
             sidebarLayout(
                 sidebarPanel(
                   h3("MetaPCA"),
                   tags$hr(),
                     h4("Summary Table"),
                     br(),br(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),                     
                     bsCollapsePanel("About", "This MetaPCA panel serves as an UI for MetaPCA package.
 MetaPCA aims to combine multiple omics datasets of identical or similar biological hypothesis and perform simultaneous dimensional reduction in all studies. The results show improved accuracy, robustness and better interpretation among all studies.",
                   a(strong("Tutorials"), href="https://github.com/metaOmics/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                               style = "primary"),
          bsCollapsePanel("Glossary", strong("PCA"), " principal component analysis", br(),
                                   style = "default"),
                   
                     tags$hr(),  
                     bsCollapsePanel("Advanced Options",
                       tagList(
                            selectInput(ns("methods"), label = "Methods for MetaPCA",
                                 choices = list("SSC" = "SSC", "SV" = "SV"),
                                 selected = "SSC"),
                            numericInput(ns("dim"), "Dimension of meta-eigenvector matrix:", value = 2)
            ), style="info"
          ),                                                                        
                     checkboxInput(ns("dimAuto"),"Dimension determined by variance quantile",value = TRUE),

                     checkboxInput(ns("sparse"),"Enforce Sparsity"),
                     
                     conditionalPanel(
                         condition="document.getElementById('meta_pca-sparse').checked == true ",
                         fluidRow(
                             column(3,numericInput(ns("min"),"Min lambda:", value=1)),
                             column(4,numericInput(ns("max"),"Max lambda:", value=10)),
                             column(4,numericInput(ns("step"),"Step of lambda:", value=1)),
                             helpIcon("step_pcaLambda_help",HELP.step.pcaLambda)
                         ),
                          actionButton(ns("tuneGo"), "Search for optimal tuning parameter", icon=icon("line-chart"),class="btn-danger"),
                         br(),
                         tags$hr(),
                         numericInput(ns("lambda"), "Tuning parameter for sparsity:", value = 6)
                     ),

                     tags$hr(),
                     
                     actionButton(ns("pcaGo"), "Run meta PCA",icon = icon("rocket"),class="btn-success btn-run")
                 ),
                 mainPanel(
                     h4("Meta PCA plots"),
                     tags$hr(),
                     uiOutput(ns("plots")),
                     h4("Tune sparsity parameter diagnostic plot"),
                     tags$hr(),
                     plotOutput(ns("tuningPlot"))
                 )
             )
             )
}
