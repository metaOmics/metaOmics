meta_clust_ui <- function(id, label = "MetaClust") {
    ns <- NS(id)
    tabPanel("MetaClust", value=id,
             sidebarLayout(
                 sidebarPanel(
                     h4("Summary Table"),
                     br(),br(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),
                     bsCollapse(id = "collapseExample", open = c("About","Run Meta Sparse K-Means"), multiple=TRUE,
                                bsCollapsePanel("About", "This metaClust panel serves as an UI for MetaSparseKmeans.
Starting with multiple studies, we could run MetaSparseKmeans with pre-specified number of clusters (K) and gene selection tuning parameter (Wbounds).
If you are not sure about what are good K and Wbounds, please try Tune K and Tune Wbounds panel.
Details are in ",
                                                a(strong("Tutorials"),     href="https://github.com/metaOmic/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                                                style = "primary"),
                                bsCollapsePanel("Tune K (optional)",
                                                "This panel is for tuning number of clusters K",
                                                tags$hr(),
                                                numericInput(ns("maxK"), "Maximum of K:", value=5),
                                                selectizeInput(ns("studyforK"), label = "Select studies to be tuned", choices = NULL,multiple=TRUE),
                                bsCollapsePanel("Advanced Options",
                                  tagList(
                                    numericInput(ns("topPerc"),"Top percentage by larger variance:",value =0.1),
                                    numericInput(ns("BforK"), "Number of permutations",value=10) 
                     ), style="info"
                  ),  
                                    tags$hr(),                                                                                                   
                              actionButton(ns("tuneK"), "Tune K",icon = icon("refresh", lib = "font-awesome")),
                                                helpIcon("tune_k_help",HELP.tune.k),
                                                style = "primary"),
                                bsCollapsePanel("Tune Wbounds (optional)",
                                                "This panel is for tuning Wbounds",
                                                tags$hr(),
                                                numericInput(ns("KforW"),"Number of clusters for tuning wbounds", value=3),                                        
                              bsCollapsePanel("Advanced Options",
                               tagList(
                                    numericInput(ns("BforW"), "Iterations:", value=2), 
                                       fluidRow(
                                               column(3,numericInput(ns("minforW"),"Minimum of wbounds:", value=2)),
                                               column(4,numericInput(ns("maxforW"),"Maximum of wbounds:", value=16)),
                                               column(4,numericInput(ns("stepforW"),"Step of wbounds:", value=2)),
                                               helpIcon("step_w_help",HELP.step.w)
                                       )                                    
                     ), style="info"
                  ),   
                                                tags$hr(),       
                                                actionButton(ns("tuneW"), "Tune wbounds",icon = icon("refresh",lib="font-awesome")),
                                                helpIcon("tune_w_help",HELP.tune.w),
                                                style="primary"),

                                bsCollapsePanel("Run Meta Sparse K-Means",
                                                "This panel is for running meta sparse K means. Not optional (obviously)",
                                                tags$hr(),
                                                numericInput(ns("k"), "Number of clusters:",
                                                             value = 5),            
                                                numericInput(ns("wBounds"), "Wbounds:",
                                                             value = 10),
                                bsCollapsePanel("Advanced Options",
                                   tagList(                                                             
                                                fluidRow(
                                                    column(10,
                                                selectInput(ns("methods"), label = "Methods for meta sparse K means",
                                                            choices = list("Exhaustive" = "exhaustive", "Linear" = "linear", "MCMC" = "MCMC"),
                                                            selected = "exhaustive")
                                                           ),
                                                    column(1,helpIcon("metaClust_methods_help",HELP.meta.clust.methods))
                                                ),

                                                fluidRow(
                                                    column(5,checkboxInput(ns("sizeAdj"),"Adjust sample size",FALSE)),
                                                    column(1,helpIcon("metaClust_sizeAdj_help",HELP.meta.clust.sizeAdj))
                                                )
                     ), style="info"
                  ),                                                   
                                              
                                                br(),
                                                actionButton(ns("clustGo"), "Run meta sparse K means",icon = icon("rocket"),class="btn-success btn-runA"),
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
