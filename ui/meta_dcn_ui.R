meta_dcn_ui <- function(id, label = "MetaNetwork") {
  ns <- NS(id)
  tabPanel("MetaNetwork", value=id,
    sidebarLayout(
      sidebarPanel(
         h4("Summary Table"),
          br(),br(),
          tableOutput(ns("summaryTable")),
          tags$hr(),
          bsCollapsePanel("About", "This MetaNetwork panel serves as an UI for MetaNetwork package.
 MetaNetwork aims at differential network detection and combines multiple studies to identify stable and accurate differential co-expression modules between disease conditions across studies. It includes three steps to get differentially co-expressed networks: generate network, search for basic modules, and assemble supermodules.",
                   a(strong("Tutorials"), href="https://github.com/metaOmic/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank"),
                               style = "primary"),
         tags$hr(),             
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
