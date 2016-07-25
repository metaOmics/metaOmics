saved_data_ui <- function(id, label = "saved data of single study or multiple study") {
  ns <- NS(id)
  tabPanel("Saved Data", value=id,
    sidebarLayout(
      sidebarPanel(
        h3("Selected Datasets"),
	textOutput(ns("selected")),
	tags$hr(),
        h3("Merging and Filtering Datasets"),
        uiOutput(ns("merge.option")),
	tags$hr(),
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
