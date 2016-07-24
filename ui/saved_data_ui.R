saved_data_ui <- function(id, label = "saved data of single study or multiple study") {
  ns <- NS(id)
  tabPanel("Saved Data", value=id,
    sidebarLayout(
      sidebarPanel(
        h3("Selected Datasets"),
	textOutput(ns("selected")),
	tags$hr(),
        h3("Merging and Filtering Datasets"),
	sliderInput("precMean", "mean:", 
	  min = 0, max = 1, value = 0.3, step= 0.01),
	sliderInput("precVar", "variance:", 
	  min = 0, max = 1, value = 0.3, step= 0.01),
        numericInput("threshold", "threshold", 1, min=0),
	tags$hr(),
        actionButton(ns('merge'),  'Merge from Selected Datasets', icon=icon("compress")),
        h3("Delete Data"),
        actionButton(ns('delete'), 'Delete Selected Data', icon=icon("trash"))
      ),
      mainPanel(
        h3("List of saved data"),
        fluidRow(
          column(4,
            selectInput(ns("ntype"), "numeric type", study.ntype)
          ),
          column(4, 
            selectInput(ns("stype"), "study type:", study.stype)
          ),
          column(4,
            selectInput(ns("dtype"), "data type", study.dtype)
          )
        ),
        # Create a new row for the table.
        fluidRow(
          DT::dataTableOutput(ns("table"))
        )
      )
    )
  )
}
