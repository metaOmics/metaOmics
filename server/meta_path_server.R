meta_path_server <- function(input, output, session) {
  library(MetaPath)
  data(pathways)

  ns <- NS("meta_path")

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  DE <- reactiveValues(result=NULL)

  ##########################
  # Validation             #
  ##########################
  validate <- function() {
    if(length(DB$active) == 0 )
      warning(MSG.no.active)
  }

  ##########################
  # Observers              #
  ##########################
  observeEvent(input$tabChange, {
    DB$active <- DB.load.active(db)
    tryCatch({
      file.path <- paste(DB.load.working.dir(db), "Meta DE", "result.rds", sep="/")
      DE$result <- readRDS(file.path)
      sendInfoMessage(session, MSG.detect.de.result)
    })
  })


  ##########################
  # Render output/UI       #
  ##########################
  output$srcSelect <- renderUI({
    if (!is.null(DE$result)) {
      radioButtons(ns("useDE"), 'Use Meta DE Result:', inline=T,
        c(Yes=T, No=F), T
      )
    }
  })

  output$method.opt <- renderUI({
    if (input$method == DE.METHOD.MAPE) {
      selectizeInput(ns('stat'), "meta p-value method:", MAPE.STAT.all)
    }
  })

  output$stat.opt <- renderUI({
    if (length(input$stat) > 0 && input$stat == MAPE.STAT.rth) {
      numericInput(ns("rth.value"), "rth value", NULL)
    }
  })

  output$enrichment.opt <- renderUI({
    if (input$enrichment == ENRICHMENT.KS) {
      radioButtons(ns("permute"), "Permutation to get p-value",
                   c(YES=T, No=F), T)
    } else if (input$enrichment == ENRICHMENT.fisher) {
      numericInput(ns("Degene.number"), "number of DE genes", NULL)
    }
  })

  output$permute.opt <- renderUI({
    if (length(input$permute) > 0 && input$permute == T && 
        input$enrichment == ENRICHMENT.KS) {
      tagList(
        selectizeInput(ns('permutation'), "Permutation Method", PERMUTE.all),
        numericInput(ns("nperm"), "Number of Permutation", NULL),
        selectizeInput(ns('qvalue.cal'), "q-value Calculation Method", QVALUE.all)
      )
    }
  })
}
