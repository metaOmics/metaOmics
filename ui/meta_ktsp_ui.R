meta_ktsp_ui <- function(id, label = "meta KTSP") {
    ns <- NS(id)
    tabPanel("Meta KTSP", value=id,
             sidebarLayout(
                 sidebarPanel(
                     h4("Summary Table"),
                     br(),br(),
                     tableOutput(ns("summaryTable")),
                     tags$hr(),
                     selectInput(ns("methods"), label = "Methods for Meta KTSP",
                                 choices = list("Mean score" = "Mean score", "Fisher" = "Fisher", "Stouffer" = "Stouffer"),
                                 selected = "Mean score"),
                     br(),
                     numericInput(ns("kMax"), "Max number of top scoring pairs (K)", value=9),
                     br(),
                     checkboxInput(ns("vo"),"Whether K selection is variance optimization (VO)", value=TRUE),
                     selectizeInput(ns("twoLabels"), label = "Please select only two labels to train", choices = NULL,multiple=TRUE),
                     tags$hr(),
                     actionButton(ns("ktspGo"),"Run meta KTSP", icon= icon("rocket"),class="btn-success btn-run")
                 ),
                 mainPanel(
                     h4("Differently labeled samples"),
                     tags$hr(),
                     tableOutput(ns("diffTable"))
                 )
             )
             )
}
