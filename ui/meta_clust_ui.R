meta_clust_ui <- function(id, label = "meta sparse K means") {
    ns <- NS(id)
    tabPanel("MetaClust", value=id,
             sidebarLayout(
                 sidebarPanel(
                     h4("Summary Table"),
                     tags$hr(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),
                     bsCollapse(id = "collapseExample", open = c("About","Run Meta Sparse K-Means"), multiple=TRUE,
                                bsCollapsePanel("About", "This is a panel with just text: Add text here ",  style = "primary"),
                                bsCollapsePanel("Tune K (optional)",
                                                "This panel is for tuning number of clusters K",
                                                tags$hr(),
                                                numericInput(ns("maxK"), "Maximum of K:", value=10),
                                                numericInput(ns("topPerc"),"Top percentage by larger variance:",value =0.1),
                                                numericInput(ns("BforK"), "Number of permutations",value=10),
                                                br(),

                                                selectizeInput(ns("studyforK"), label = "Select studies to be tuned", choices = NULL,multiple=TRUE),
                                                actionButton(ns("tuneK"), "Tune K for all studies",icon = icon("duplicate", lib = "glyphicon")),                           
                                                style = "primary"),
                                bsCollapsePanel("Tune Wbounds (optional)",
                                                "This panel is for tuning Wbounds",
                                                tags$hr(),
                                                numericInput(ns("KforW"),"Number of clusters for tuning wbounds", value=3),
                                                numericInput(ns("BforW"), "Iterations:", value=2),
                                                numericInput(ns("byW"),"Step of wbounds:", value=2),
                                                sliderInput(ns("WRange"), "Range of wbounds:",
                                                            min = 0, max = 30, value = c(1,15),step=2),
                                                actionButton(ns("tuneW"), "Tune wbounds",icon = icon("stats", lib = "glyphicon")),                              
                                                style="primary"),

                                bsCollapsePanel("Run Meta Sparse K-Means",
                                                "This panel is for running meta sparse K means. Not optional (obviously)",
                                                tags$hr(),
                                                numericInput(ns("k"), "Number of clusters:",
                                                             value = 3),            
                                                numericInput(ns("wBounds"), "Wbounds:",
                                                             value = 10),
                                                selectInput(ns("methods"), label = "Methods for meta sparse K means",
                                                            choices = list("Exhaustive" = "exhaustive", "Linear" = "linear", "MCMC" = "MCMC"),
                                                            selected = "exhaustive"),
                                                checkboxInput(ns("sizeAdj"),"Adjust sample size",FALSE),
                                                br(),
                                                actionButton(ns("clustGo"), "Run meta sparse K means",icon = icon("play", lib = "glyphicon")),
                                                style="primary")
                                )

                 ),
                 mainPanel(
                     h4("Heatmap"),
                     tags$hr(),
                     uiOutput(ns("heatmaps")),
                     h4("Gap statistics (K)"),
                     tags$hr(),
                     uiOutput(ns("plotsK")),
                     h4("Gap statistics (wbounds)"),
                     tags$hr(),
                     plotOutput(ns("plotW"))
                 )
             )
             )
}
