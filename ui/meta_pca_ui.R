meta_pca_ui <- function(id, label = "meta PCA") {
    ns <- NS(id)
    tabPanel("MetaPCA", value=id,
             sidebarLayout(
                 sidebarPanel(
                 
                     h4("Parameters for MetaPCA"),
                     br(),
                     selectInput(ns("methods"), label = "Methods for MetaPCA",
                                 choices = list("Fisher" = "Fisher", "SSC" = "SSC", "SV" = "SV"),
                                 selected = "Fisher"),
                     br(),

                     sliderInput(ns("dim"), "Dimension of meta-eigenvector matrix:",
                                 min = 0, max = 30, value = 2, step = 1),
                     br(),

                     
#                     selectInput(ns("dimAuto"), label = "Whether dimension is determined by variance quantile",
#                                 choices = list("True" ="TRUE", "False"="FALSE"),
                                        #                                 selected = "TRUE"),
                     checkboxInput(ns("dimAuto"),"Dimension determined by variance quantile"),

                     checkboxInput(ns("sparse"),"Sparseness encouraged"),
                     
#                     selectInput(ns("sparse"), label = "Whether sparseness is encouraged",
#                                 choices = list("True" ="TRUE", "False"="FALSE"),
#                                 selected = "TRUE"),
                     conditionalPanel(
                         condition="document.getElementById('meta_pca-sparse').checked == true ",
                         sliderInput(ns("lambda"), "Tuning parameter for sparsity:",
                                     min = 0, max = 20, value = 6, step = 1)
                     ),

                     tags$hr(),
                     
                     actionButton(ns("pcaGo"), "Run meta PCA",icon = icon("play", lib = "glyphicon"))
                 ),
                 mainPanel(
                     h4("Meta PCA plots"),
                     tags$hr(),
                     uiOutput(ns("plots"))
                     )
                 )
             )
}
