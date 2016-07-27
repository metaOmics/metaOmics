saved_data_ui <- function(id, label = "saved data of single study or multiple study") {
  ns <- NS(id)
  tabPanel("Saved Data", value=id,
    sidebarLayout(
      sidebarPanel(
        h4("Selected Datasets"), helpIcon(ns('merge_select'), HELP.select.datasets),
	textOutput(ns("selected")),
	tags$hr(),
        h4("Merging and Filtering Datasets"), helpIcon(ns('merge_help'), HELP.merge),
        uiOutput(ns("merge.option")),
	tags$hr(),
        h4("Danger Zone: "), helpIcon(ns('delete_help'), HELP.delete), tags$br(),
        actionButton(ns('delete'), 'Delete Selected Data', icon=icon("trash"), class="btn-danger")
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
        DT::dataTableOutput(ns("table"))
      )
    ),
    uiOutput(ns("active"))
  )
}
