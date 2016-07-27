meta_clust_ui <- function(id, label = "meta sparse K means") {
  ns <- NS(id)
  tabPanel("MetaClust", value=id,
    sidebarLayout(
        sidebarPanel(
            ##############################
            # Parameters for SparsKMeans #
            ##############################
            h4("Selected parameters for meta sparse K mean"),
            sliderInput(ns("k"), "Number of Clusters:",
                        min = 0, max = 20, value = 3, step= 1),            
            sliderInput(ns("wBounds"), "W bounds:",
                        min = 0, max = 50, value = 10, step= 1),           
            numericInput(ns("lambda"), "Lambda:", value = 2),
            actionButton(ns("clustGo"), "Run meta sparse K means")
      ),
      mainPanel(
          h3("Heatmap"),
          fluidRow(
             plotOutput(ns("heatmap"))
          )
      )
    )
 )
}
