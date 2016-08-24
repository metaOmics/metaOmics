meta_clust_ui <- function(id, label = "meta sparse K means") {
    ns <- NS(id)
    tabPanel("MetaClust", value=id,
             sidebarLayout(
                 sidebarPanel(
                     h4("Summary Table"),
                     tags$hr(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),
                     bsCollapse(id = "collapseExample", open = "About",
                                bsCollapsePanel("About", "This is a panel with just text: Add text here ",  style = "danger"),
                                bsCollapsePanel("Tune K",
                                                "This panel is for tuning number of clusters K (optional) ",
                                                tags$hr(),
                                                numericInput(ns("maxK"), "Maximum of K:", value=10),
                                                numericInput(ns("topPerc"),"Top percentage by larger variance:",value =0.1),
                                                numericInput(ns("BforK"), "Number of permutations",value=10),
                                                br(),
                                                numericInput(ns("KIndi"), "Tune K for individual study no. ", value=1),
                                                actionButton(ns("tuneKIndi"), "Tune K for individual study: ",icon = icon("file", lib = "glyphicon")),
                                                h6("Or"),
                                                actionButton(ns("tuneK"), "Tune K for all studies",icon = icon("duplicate", lib = "glyphicon")),                           
                                                style = "danger"),
                                bsCollapsePanel("Tune Wbounds",
                                                "This panel  is for tuning Wbounds (optional)",
                                                tags$hr(),
                                                numericInput(ns("KforW"),"Number of clusters for tuning wbounds", value=3),
                                                numericInput(ns("B"), "Iterations:", value=2),
                                                numericInput(ns("byW"),"Step of wbounds:", value=2),
                                                sliderInput(ns("WRange"), "Range of wbounds:",
                                                                            min = 0, max = 30, value = c(3,15),step=2),
                                                actionButton(ns("tuneW"), "Tune wbounds",icon = icon("stats", lib = "glyphicon")),                              
                                                style="danger")
                                ),
                     
##############################
                                        # Parameters for SparsKMeans #
##############################
                     h4("Selected parameters for meta sparse K mean"),
                     numericInput(ns("k"), "Number of clusters:",
                                 value = 3),            
                     numericInput(ns("wBounds"), "Wbounds:",
                                  value = 10),
                                        #          numericInput(ns("lambda"), "Lambda:", value = 2),
                     actionButton(ns("clustGo"), "Run meta sparse K means",icon = icon("play", lib = "glyphicon"))
                 ),
                 mainPanel(
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
