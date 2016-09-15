meta_pca_ui <- function(id, label = "meta PCA") {
    ns <- NS(id)
    tabPanel("MetaPCA", value=id,
             sidebarLayout(
                 sidebarPanel(
                     h4("Summary Table"),
                     br(),br(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),
                     selectInput(ns("methods"), label = "Methods for MetaPCA",
                                 choices = list("SSC" = "SSC", "SV" = "SV"),
                                 selected = "SSC"),
                     br(),

                     numericInput(ns("dim"), "Dimension of meta-eigenvector matrix:", value = 2),
                     br(),

                     checkboxInput(ns("dimAuto"),"Dimension determined by variance quantile",value = TRUE),

                     checkboxInput(ns("sparse"),"Sparsity encouraged"),
                     
                     conditionalPanel(
                         condition="document.getElementById('meta_pca-sparse').checked == true ",
                         numericInput(ns("lambda"), "Tuning parameter for sparsity:", value = 6),
                         actionButton(ns("tuneGo"), "Search for optimal tuning parameter", icon=icon("line-chart"),class="btn-danger")
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
