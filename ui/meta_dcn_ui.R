meta_dcn_ui <- function(id, label = "meta DCN") {
  ns <- NS(id)
  tabPanel("Meta DCN", value=id,
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('caseName')),
        uiOutput(ns('controlName')),
        numericInput(ns("permutationTimes"), "Number of Permutations:", 
          value=3),
        numericInput(ns("repeatTimes"), "Number of Repeats:", 
          value=3),
        uiOutput(ns("CPUNumbersButton")),
        #numericInput(ns("CPUNumbers"), "Number of CPUs:", 
        #  value=1),
        sliderInput(ns("FDRCutoff"), label="FDR Cutoff", value=0.3, min=0, max=1),
        sliderInput(ns("edgeCutoff"), label="Edge Cutoff", value=0.1, 
          min=0, max=1),
        tags$hr(),
        actionButton(ns('run'), 'Run', icon=icon("rocket"), class="btn-success btn-run")
        ),
      mainPanel(
        h3(textOutput(ns("HModuleHeader"))),
        br(),
        DT::dataTableOutput(ns("HModuleTable")),
        fluidRow( 
          column(8, br(),h4(textOutput(ns("HModuleText")))),
          column(2, uiOutput(ns("HModuleSelect")))
          ),
        imageOutput(ns("HMImage"), height = 500),
        textOutput(ns("src")),
        tags$hr(),
        h3(textOutput(ns("LModuleHeader"))),
        br(),
        DT::dataTableOutput(ns("LModuleTable")),
        fluidRow(
          column(8, br(),h4(textOutput(ns("LModuleText")))),
          column(2, uiOutput(ns("LModuleSelect")))
          ),
        imageOutput(ns("LMImage"), height = 500),
        textOutput(ns("src2")),
        tags$hr(),
        h3("MetaDCN pathway-guided supermodules"),
        DT::dataTableOutput(ns("supermodule"))
      )
    )
  )
}
