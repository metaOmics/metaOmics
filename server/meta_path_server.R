meta_path_server <- function(input, output, session) {
  library(MetaPath)
  data(pathways)

  ns <- NS("meta_path")

  getOption <- function(input) {
    opt <- list()
    if (is.null(DE$result) || (length(input$useDE) > 0 && input$useDE == F)) {
      opt$arraydata <- DB$active@datasets
      opt$resp.type <- input$resp.type
    } else {
      opt$meta.p <- DE$result$meta.analysis$pval
      opt$ind.p <- DE$result$ind.p
      opt$MetaDE <- T
    }
    opt$pathway     <- lapply(input$pathway, function(name) get(name))
    opt$method      <- input$method
    opt$enrichment  <- input$enrichment

    if (input$method == DE.METHOD.MAPE)
      opt$stat <- input$stat
    if (length(input$stat) > 0 && input$stat == MAPE.STAT.rth)
      opt$rth.value <- input$rth.value

    if (input$enrichment == ENRICHMENT.KS) {
      opt$permute  <- input$permute
    } else if (input$enrichment == ENRICHMENT.fisher) {
      opt$Degene.number <- input$Degene.number
    }

    if (length(input$permute) > 0 && input$permute == T && 
        input$enrichment == ENRICHMENT.KS) {
      opt$permutation <- input$permutation
      opt$nperm <- input$nperm
      opt$qvalue.cal <- input$qvalue.cal
    }

    opt$size.min <- input$size.min
    opt$size.max <- input$size.max
    opt
  }

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
    }, error=function(error){
      sendInfoMessage(session, MSG.no.de.result)
    })
  })

  observeEvent(input$run, {
    try({
      do.call(MAPE2.0, getOption(input))
    }, session)
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

  output$resp.opt <- renderUI({
    if(is.null(DE$result) || (length(input$useDE) > 0 && input$useDE == F))
      selectizeInput(ns('resp.type'), "Response Type:", RESP.all)
  })

  output$method.opt <- renderUI({
    if (input$method == DE.METHOD.MAPE) {
      selectizeInput(ns('stat'), "meta p-value method:", MAPE.STAT.all)
    }
  })

  output$stat.opt <- renderUI({
    if (length(input$stat) > 0 && input$stat == MAPE.STAT.rth) {
      numericInput(ns("rth.value"), "rth value", 1)
    }
  })

  output$enrichment.opt <- renderUI({
    if (input$enrichment == ENRICHMENT.KS) {
      radioButtons(ns("permute"), "Permutation to get p-value",
                   c(YES=T, No=F), F)
    } else if (input$enrichment == ENRICHMENT.fisher) {
      numericInput(ns("Degene.number"), "number of DE genes", NULL)
    }
  })

  output$permute.opt <- renderUI({
    if (length(input$permute) > 0 && input$permute == T && 
        input$enrichment == ENRICHMENT.KS) {
      tagList(
        selectizeInput(ns('permutation'), "Permutation Method", PERMUTE.all),
        numericInput(ns("nperm"), "Number of Permutation", 500),
        selectizeInput(ns('qvalue.cal'), "q-value Calculation Method", QVALUE.all)
      )
    }
  })
}
