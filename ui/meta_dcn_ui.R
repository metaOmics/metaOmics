meta_dcn_ui <- function(id, label = "meta DCN") {
  ns <- NS(id)
  tabPanel("Meta DCN", value=id,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('caseName')),
        uiOutput(ns('controlName')),
        numericInput(ns("permutationTimes"), "Number of Permutations:", 
          value=4),
        #uiOutput(ns("CPUNumbersButton")),
        sliderInput(ns("edgeCutoff"), label="Edge Cutoff", value=0.1, 
          min=0, max=1),
        tags$hr(),
        actionButton(ns('GeneNet'), 'Generate Network', icon=icon("rocket"), class="btn-success btn-run"),
        uiOutput(ns('repeatTimes')),
        uiOutput(ns('MCSteps')),
        uiOutput(ns('jaccardCutoff')),
        uiOutput(ns('SearchBM')),
        uiOutput(ns('FDRCutoff')),
        uiOutput(ns('MetaDCN'))
        ),
      mainPanel(
        h3(textOutput(ns("BMInCaseHeader"))),
        br(),
        DT::dataTableOutput(ns("BMInCaseTable")),
        fluidRow( 
          column(8, br(),h4(textOutput(ns("BMInCaseText")))),
          column(2, uiOutput(ns("BMInCaseSelect")))
          ),
        imageOutput(ns("HMImage"), height = 500),
        textOutput(ns("src")),
        tags$hr(),
        h3(textOutput(ns("BMInControlHeader"))),
        br(),
        DT::dataTableOutput(ns("BMInControlTable")),
        fluidRow(
          column(8, br(),h4(textOutput(ns("BMInControlText")))),
          column(2, uiOutput(ns("BMInControlSelect")))
          ),
        imageOutput(ns("LMImage"), height = 500),
        textOutput(ns("src2")),
        tags$hr(),
        h3(textOutput(ns("supermoduleHeader"))),
        DT::dataTableOutput(ns("supermodule"))
      )
    )
  )
}
