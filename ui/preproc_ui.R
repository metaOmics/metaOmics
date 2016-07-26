preproc_ui <- function(id, label= "preprocessing data") {
  ns <- NS(id)
  tabPanel("Preprocessing", value=id,
    sidebarLayout(
      sidebarPanel(
        ##########################
        # Choosing / Upload Data #
        ##########################
        h4("Choosing/Upload Data"),
        fileInput(ns("file"), 'Choose CSV File',
          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        selectizeInput(ns("dataset"), "Or use existing datasets", DB.ls(db),
          options = list(
            placeholder = 'Choose from below',
            onInitialize = I('function() { this.setValue(""); }'),
            onItemAdd = I('function() {reset_file()}')
          )
        ),
        tags$hr(),

        ##########################
        # Tranforming Data       #
        ##########################
        conditionalPanel(
          condition="document.getElementById('preproc-file').files.length > 0",
          h4("Transforming Data"),
          checkboxInput(ns("header"), 'Header', TRUE),
          radioButtons(ns("sep"), 'Separator', inline=T,
            c(Comma=',', Semicolon=';', Tab='\t'), '\t'
          ),
          radioButtons(ns("quote"), 'Quote for String', inline=T,
            c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'
          ),
          radioButtons(ns("log"), 'Log tranforming data', inline=T,
            c('Yes'=T, 'No'=F), T 
          ),
          tags$hr()
        ),

        ##########################
        # Annotation             #
        ##########################
        h4("Annotation"),
        selectInput(ns("id.type"), "ID type", id.type),
        uiOutput(ns("id.type.option")),
        tags$hr(),

        ##########################
        # Save and Metadata      #
        ##########################
        h4("Configuring Metadata"),
        selectInput(ns("dtype"), "Type of Data", study.dtype),
        textInput(ns("studyName"), "Study Name:", "some study"),
        actionButton(ns('saveStudy'), 'save single study', icon=icon("save"))
      ),
      mainPanel(
        h3(textOutput(ns("studyName"), container=span)),
        verbatimTextOutput(ns("summary")),
        DT::dataTableOutput(ns("dataPreview"))
      )
    )
  )
}
