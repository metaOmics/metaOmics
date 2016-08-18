meta_clust_ui <- function(id, label = "meta sparse K means") {
    ns <- NS(id)
    tabPanel("MetaClust", value=id,
             sidebarLayout(
                 sidebarPanel(
##############################
                                        # Parameters for SparsKMeans #
##############################
                     textInput(ns("outDir"), "Output directory:", "~/"),
                     h4("Tune number of clusters K (optional)"),
                     br(),
                     actionButton(ns("tuneK"), "Tune K",icon = icon("stats", lib = "glyphicon")),
                     tags$hr(),
                     h4("Tune wbounds (optional)"),
                     sliderInput(ns("KforW"), "Number of clusters for tuning wbounds:",
                                 min = 0, max = 20, value = 3, step = 1),
                     numericInput(ns("B"), "Iterations:", value=2),
                     numericInput(ns("min"), "Min of wbounds:", value=2),
                     numericInput(ns("max"), "Max of wbounds:", value=15),
                     actionButton(ns("tuneW"), "Tune wbounds",icon = icon("stats", lib = "glyphicon")),
                     tags$hr(),
                     
                     h4("Selected parameters for meta sparse K mean"),
                     sliderInput(ns("k"), "Number of clusters:",
                                 min = 0, max = 20, value = 3, step= 1),            
                     sliderInput(ns("wBounds"), "Wbounds:",
                                 min = 0, max = 50, value = 10, step= 1),           
                                        #          numericInput(ns("lambda"), "Lambda:", value = 2),
                     actionButton(ns("clustGo"), "Run meta sparse K means",icon = icon("play", lib = "glyphicon"))
                 ),
                 mainPanel(
                                        #        fluidRow(
                                        #          column(8, plotOutput("plot1")),
                                        #          column(12, plotOutput("plot2")),
                                        #        ),
                                        # This is the dynamic UI for the plots
                     h4("Gap statistics (K)"),
                     tags$hr(),
                     uiOutput(ns("plotsK")),
                     h4("Gap statistics (wbounds)"),
                     tags$hr(),
                     plotOutput(ns("plotW")),
                     h4("Heatmap"),
                     fluidRow(
                         uiOutput(ns("heatmaps"))
                                        # ui_output
                     )
                 )
             )
             )
}
