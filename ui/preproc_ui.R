preproc_ui <- function(id, label= "preprocessing data") {
  ns <- NS(id)
  tabPanel("Preprocessing", value=id,
    sidebarLayout(
      sidebarPanel(
        ##########################
        # Choosing / Upload Data #
        ##########################
        h4("Data"),
        bsCollapse(id="preproc-data",
          bsCollapsePanel("Choosing/Upload Expression Data",
            tagList(
              fileInput(ns("exprfile"), 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
              ),
              conditionalPanel(
                condition="document.getElementById('preproc-exprfile').files.length > 0",
                h4("Expression Data Parsing Option"),
                checkboxInput(ns("header"), 'Header', TRUE),
                radioButtons(ns("data.sep"), 'Separator', inline=T,
                  c(Comma=',', Semicolon=';', Tab='\t'), ','
                ),
                radioButtons(ns("data.quote"), 'Quote for String', inline=T,
                  c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'
                ),
                radioButtons(ns("log"), 'Log tranforming data', inline=T,
                  c('Yes'=T, 'No'=F), F 
                ),
                tags$hr()
              ),
              selectizeInput(ns("study"), "Or use existing datasets", DB.ls(db),
                options = select.noDefault
              )
            ), style="primary"
          ),
          bsCollapsePanel("Upload Clinical Data",
            tagList(
              fileInput(ns("clinical"), 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
              ),
              conditionalPanel(
                condition="document.getElementById('preproc-clinical').files.length > 0",
                h4("Expression Data Parsing Option"),
                radioButtons(ns("clinical.sep"), 'Separator', inline=T,
                  c(Comma=',', Semicolon=';', Tab='\t'), ','
                ),
                radioButtons(ns("clinical.quote"), 'Quote for String', inline=T,
                  c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'
                )
              )
            ), style="primary"
          )
        ),
        tags$hr(),
        
        ##########################
        # Tranforming Data       #
        ##########################        
        
        h4("Preprocessing"),
        bsCollapse(id="preproc-uplaod-clinical",
          bsCollapsePanel("Annotate / Impute / Replicate",
            tagList(
              h4("Annotation"), helpIcon(ns("annotate_help"), HELP.annotate),
              selectInput(ns("id.type"), "ID type", as.list(ID.TYPE.all)),
              uiOutput(ns("id.type.option")),
              tags$hr(),
             
              h4("Impute"), helpIcon(ns("impute_help"), HELP.impute),
              uiOutput(ns("impute.opt")),
              tags$hr(),
             
              h4("Replicate Handling"), helpIcon(ns("replicate_help"), HELP.replicate),
              uiOutput(ns("replicate.opt"))
            ), style="primary"
          )
        ),
        tags$hr(),

        ##########################
        # Save and Metadata      #
        ##########################
        h4("Saving Study"),
        selectInput(ns("dtype"), "Type of Data", as.list(DTYPE.all)),
        textInput(ns("studyName"), "Study Name:", "some study"),
        actionButton(ns('saveStudy'), 'save single study', icon=icon("save"), class="btn-success")
      ),
      mainPanel(
        h3(textOutput(ns("studyName"), container=span)),
        verbatimTextOutput(ns("summary")),
        h3("Expression Data Preview"),
        DT::dataTableOutput(ns("dataPreview")),
        h3("Clinical Data Preview"),
        DT::dataTableOutput(ns("clinicalPreview"))
      )
    )
  )
}
