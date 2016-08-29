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
        bsCollapse(id="saved_data-danger",
          bsCollapsePanel("Danger Zone",
            actionButton(ns('delete'), 'Delete Selected Data',
              icon=icon("trash"), class="btn-danger")
            , style="danger"
          )
        )
      ),
      mainPanel(
        h3("List of saved data"),
        DT::dataTableOutput(ns("table"))
      )
    ),
    uiOutput(ns("active"))
  )
}
