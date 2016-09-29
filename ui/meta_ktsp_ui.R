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
                     selectizeInput(ns("twoLabels"), label = "Please select TWO labels to train", choices = NULL,multiple=TRUE),
                     tags$hr(),
                     selectizeInput(ns("trainStudy"), label = "Please select studies for training", choices = NULL,multiple=TRUE),
                     br(),
                     selectizeInput(ns("testStudy"), label = "Please select ONE study for testing", choices = NULL,multiple=TRUE),
                     br(),
                     actionButton(ns("ktspGo"),"Run meta KTSP", icon= icon("rocket"),class="btn-success btn-run")
                 ),
                 mainPanel(
                     h4("Gene pair table"),
                     tableOutput(ns("genePairTable")),
                                          tags$hr(),
                     h4("Preferred table"),
                     tableOutput(ns("genePairTable2")),
                                          tags$hr(),
                     h4("Differently labeled samples"),
                     tableOutput(ns("diffTable")),
                     tags$hr(),
                     h4("K diagnostic plot"),
                     plotOutput(ns("voPlot"))
                 )
             )
             )
}
