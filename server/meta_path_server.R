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
    opt$enrichment  <- input$enrichment

    if (length(input$method) > 0) {
      opt$method      <- input$method
      if (input$method == MAPE.MAPE) {
	opt$stat <- input$stat
	if (input$stat == MAPE.STAT.rth)
	  opt$rth.value <- input$rth.value
      }
    } else if (length(input$useDE) == 0 || input$useDE == F) {
      opt$method <- MAPE.CPI
    }

    if (input$enrichment == ENRICHMENT.KS) {
      opt$permute  <- input$permute
    } else if (input$enrichment == ENRICHMENT.fisher) {
      opt$DEgene.number <- input$DEgene.number
    }

    if (length(input$permute) > 0 && input$permute == T && 
        input$enrichment == ENRICHMENT.KS) {
      opt$permutation <- input$permutation
      opt$nperm <- input$nperm
      opt$qvalue.cal <- input$qvalue.cal
    }

    opt$size.min <- input$size.min
    opt$size.max <- input$size.max
    tmp <- opt
    tmp$arraydata <- NULL
    tmp$clinical.data <- NULL
    tmp$pathway <- NULL
    print(tmp)

    opt
  }

  getKappaOption <- function(input) {
    opt <- list()
    result <- MAPE$result
    opt$q_cutoff <- input$q_cutoff
    opt$summary <- result$summary
    opt$software <- result$method
    opt$pathway <- result$pathway
    opt$max_k <- 20
    opt$output_dir <- DB$working
    if (MAPE$result$method == MAPE.MAPE) {
      opt$method <- input$kappa.method
    }

    opt
  }

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db), working=NULL)
  DE <- reactiveValues(result=NULL)
  MAPE <- reactiveValues(result=NULL, diagnostics=NULL)

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
      DE$result <- NULL
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
    wait(session, "Plotting consensus CDF and Delta area")
    tryCatch({
      MAPE$diagnostics <- do.call(MAPE.Kappa, getKappaOption(input))
    }, error=function(error){
      if(error$message == "Number of clusters 'k' must be in {1,2, .., n-1}; hence n >= 2")
        sendErrorMessage(session, MSG.too.few.pathway)
      else
        sendErrorMessage(session, error$message)
    })

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

  observeEvent(input$cluster, {
    result <- MAPE$result
    diagnostics <- MAPE$diagnostics
    wait(session, "Clustering, the result will be output in working directory")
    MAPE.Clustering(summary=result$summary,
                    Num_Clusters=input$Num_Clusters,
                    Num_of_gene_lists=result$Num_of_gene_lists,
                    genelist=genelist,
                    kappa.result=diagnostics$kappa,
                    pathway=result$pathway,
                    enrichment=result$enrichment,
                    method=diagnostics$method,
                    software=result$method,
                    output_dir=DB$working)
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
    if (length(input$useDE) == 0 || input$useDE == F) {
      selectizeInput(ns('method'), "Software:", MAPE.all)
    }
  })

  output$mape.opt <- renderUI({
    if (length(input$method) > 0 && input$method == MAPE.MAPE) {
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
      numericInput(ns("DEgene.number"), "number of DE genes", NULL)
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
    if (!is.null(MAPE$result)) {
      tagList(
        if (MAPE$result$method == MAPE.MAPE) {
          selectInput(ns("kappa.method"), "Select Cutoff Method", KAPPA.METHOD.all)
	} else {""},
        numericInput(ns("q_cutoff"), "FDR cut off value", 0.1),
        textOutput(ns("pathwayLeft"), container=div),
        actionButton(ns('plot'), 'Pathway Clustering Diagnostics', 
                    icon=icon("paint-brush"), class="btn-success btn-run lower-btn")
      )
    } else {
      h4("You need to run step 1 first")
    }
  })

  output$clustering.opt <- renderUI({
    if (!is.null(MAPE$diagnostics)) {
      tagList(
        numericInput(ns("Num_Clusters"), "Number Of Clusters", 6, min=2),
        actionButton(ns('cluster'), 'Get Clustering Result', 
                    icon=icon("paint-brush"), class="btn-success btn-run lower-btn")
      )
    } else {
      h4("You need to run step 2 first")
    }
  })
  
  output$pathwayLeft <- renderText({
    if (!is.null(MAPE$result)) {
      left <- 0
      if (MAPE$result$method == MAPE.MAPE) {
        left <- sum(MAPE$result$summary[input$kappa.method] <= input$q_cutoff)
      } else {
        left <- sum(MAPE$result$summary["q_value_meta"] <= input$q_cutoff)
      }
      paste(left, "pathways left after cutoff")
    }
  })

}
