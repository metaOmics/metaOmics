meta_path_server <- function(input, output, session) {
  library(MetaPath)
  data(pathways)

  ns <- NS("meta_path")

  getOption <- function(input) {
    opt <- list()
    if (is.null(DE$result) || (length(input$useDE) > 0 && input$useDE == F)) {
      opt$arraydata <- DB$active@datasets
      opt$clinical.data <- DB$active@clinicals
      opt$resp.type <- input$resp.type
      opt$label <- input$label
      if (input$resp.type == RESP.survival) {
        opt$censoring <- input$censoring
      }
    } else {
      opt$meta.p <- DE$result$meta.analysis$pval
      opt$ind.p <- DE$result$ind.p
      opt$MetaDE <- T
    }
    opt$pathway <- c()
    for (pathway in input$pathway) {
      opt$pathway <- c(opt$pathway, get(pathway))
    }
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
  DB <- reactiveValues(active=DB.load.active(db), working=NULL)
  DE <- reactiveValues(result=NULL)
  MAPE <- reactiveValues(result=NULL)

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
    DB$working <- paste(DB.load.working.dir(db), "Meta PATH", sep="/")
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
      wait(session, "performing Meta Path Analysis")
      MAPE$result <- do.call(MAPE2.0, getOption(input))
      file.path <- paste(DB$working, "result.rds", sep="/")
      saveRDS(MAPE$result, file=file.path)
      sendSuccessMessage(session, paste("result raw data written to", file.path))
      done(session)
    }, session)
  })

  observeEvent(input$plot, {

    result <- MAPE$result
    q_cutoff <- input$q_cutoff
    wait(session, "Plotting consensus CDF and Delta area")
    try({
      MAPE.kappa_result = MAPE.Kappa(summary=result$summary, software=result$method,
	pathway=result$pathway, max_k=20, q_cutoff=q_cutoff, output_dir=DB$working)
    }, session)

    output$consensus <- renderImage({
      img.src <- paste(DB$working, "consensus021.png", sep="/")
      list(src=img.src, contentType='image/png', alt="consensus plot")
    })

    output$delta <- renderImage({
      img.src <- paste(DB$working, "consensus022.png", sep="/")
      list(src=img.src, contentType='image/png', alt="delta area plot")
    })
    done(session)
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
    if(is.null(DE$result) || (length(input$useDE) > 0 && input$useDE == F)) {
      tagList(
        selectizeInput(ns('resp.type'), "Response Type:", RESP.all),
        uiOutput(ns("resp.type.option"))
      )
    }
  })

  output$resp.type.option <- renderUI({
    if(length(input$resp.type) > 0) {
      resp <- input$resp.type
      clinical.options <- names(DB$active@clinicals[[1]])
      if (resp == RESP.two.class || resp == RESP.multi.class
          || resp == RESP.continuous) {
        selectInput(ns("label"), "Label Attribute:", clinical.options)
      } else if (resp == RESP.survival) {
        tagList(
          selectInput(ns("label"), "Time Attribute:", clinical.options),
          selectInput(ns("censoring"), "Indicator Attribute:", clinical.options)
        )
      }
    }
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

  output$summary <- DT::renderDataTable(DT::datatable({
    if (!is.null(MAPE$result))
      MAPE$result$summary
  }))

  output$heatmap.opt <- renderUI({
    if (!is.null(MAPE$result))
      fluidRow(
        column(4, numericInput(ns("q_cutoff"), "FDR cut off", 0.1)),
        column(8, actionButton(ns('plot'), 'Plot (consensus CDF / Delta area)', 
                    icon=icon("paint-brush"), class="btn-success btn-run lower-btn")
        )
      )
  })

}
