meta_de_server <- function(input, output, session) {
  library(MetaDE)
  data(pathways) ###

  ns <- NS("meta_de")

  getOption <- function(input) {

    study <- DB$active
    n <- length(study@datasets)
    opt <- list(ind.method=rep(IND.limma, n), covariate=NULL, rth=NULL, select.group=NULL , ref.level=NULL, REM.type=NULL)

    opt$size.min <- input$size.min 
    opt$size.max <- input$size.max 

    opt$pathway <- c() 
    for (pathway in input$pathway) {
      opt$pathway <- c(opt$pathway, get(pathway))
    }
    opt$enrichment  <- input$enrichment

    if (input$enrichment == ENRICHMENT.fisher) {
      opt$DEgene.number <- input$DEgene.number
      opt$p.cut <- input$p.cut 
    }

    opt$data <- study@datasets
    opt$clin.data <- study@clinicals
    opt$data.type <- study@dtype
    method <- input$meta.method
    opt$meta.method <- method
    if (method == META.roP || method == META.roP.OC) {
      opt$rth <- input$rth
    } else if (method == META.AW) {
      if (length(input$AW.type) == 0)
        opt$AW.type <- AW.original
      else
        opt$AW.type <- input$AW.type
    } else if (method == META.REM) {
      if (length(input$REM.type) == 0)
        opt$REM.type <- REM.HO
      else
        opt$REM.type <- input$REM.type
    }
    
    opt$paired <- unlist(lapply(1:n, function(index) {
      tag.id <- paste("paired", index, sep="")
      if (length(input[[tag.id]]) == 0)
        F
      else
        input[[tag.id]] == T
    }))

    if (input$meta.type == META.TYPE.p) {
      opt$ind.method <- unlist(lapply(1:n, function(index) {
        tag.id <- paste("ind", index, sep="")
        if (length(input[[tag.id]]) == 0)
          IND.limma
        else
          input[[tag.id]]
      }))
    } 

    resp <- input$resp.type
    clinical.options <- names(DB$active@clinicals[[1]])
    labels <- DB$active@clinicals[[1]][,clinical.options[1]]
    labels <- levels(as.factor(labels))

    opt$resp.type <- resp
    if (resp == RESP.two.class) {
      if (length(input$label.col) > 0) {
        opt$response <- input$label.col
        opt$select.group <- c(input$control.label, input$expr.label)
        opt$ref.level <- input$control.label
      } else {
        opt$response <- clinical.options[1]
        opt$select.group <- c(labels[1], labels[2])
        opt$ref.level <- labels[1]
      }
    } else if (resp == RESP.multi.class) {
      if (length(input$label.col) > 0) {
        opt$response <- input$label.col
        opt$select.group <- input$multi.class.col
        opt$ref.level <- input$control.label
      } else {
        opt$response <- clinical.options[1]
        opt$select.group <- labels
        opt$ref.level <- labels[1]
      }
    } else if (resp == RESP.continuous) {
      if (length(input$label.col) > 0) {
        opt$response <- input$conti.col
      } else {
        opt$response <- clinical.options[1]
      }
    } else if (resp == RESP.survival) {
      if (length(input$time.col) > 0 && length(input$indicator.col)) {
        opt$response <- c(input$time.col, input$indicator.col)
      } else {
        opt$response <- c(clinical.options[1], clinical.options[2])
      }
    }

    if (length(input$covariate) == 0 || input$covariate == "None") {
    } else {
      opt$covariate <- input$covariate
    }

    if (length(input$parametric) > 0)
      opt$parametric <- (input$parametric == T)
    if (length(input$tail) > 0)
      opt$tail <- input$tail
    if (length(input$nperm) == 0)
      opt$nperm <- input$nperm

    tmp <- opt
    tmp$clin.data <- NULL
    tmp$data <- NULL
    print(tmp)
    opt
  }

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  DE <- reactiveValues(result=NULL, summary=NULL)

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
    DB$working.dir <- DB.load.working.dir(db)
    DB$transpose <- lapply(DB$active@datasets,t)
  })
  
  observeEvent(input$run, {
    wait(session, "running meta DE, should be soon")
    try({
      if (input$meta.method == META.AW) {
        DE$result <- posthoc.aw(do.call(MetaDE, getOption(input)))
      } else {
        DE$result <- do.call(MetaDE, getOption(input))
      }
      DE$summary <- summary.meta(DE$result, input$meta.method)
      dir.path <- paste(DB.load.working.dir(db), "MetaDE", sep="/")
      if (!file.exists(dir.path)) dir.create(dir.path)
      file.path <- paste(dir.path, "summary.csv", sep="/")
      write.csv(DE$summary, file=file.path)
      sendSuccessMessage(session, paste("summary file written to", file.path))
      file.path <- paste(dir.path, "result.rds", sep="/")
      saveRDS(DE$result, file=file.path)
      sendSuccessMessage(session, paste("raw data written to", file.path), unique=T)
    }, session)
    done(session)
  })

  observeEvent(input$plot, {
    outfile <- tempfile(fileext='.png')
    height <- 800 * input$scale
    width <- 400 * length(DB$active@datasets) * input$scale
    wait(session, paste("Plotting result with scale:", input$scale))
    try({
      png(outfile, res=120, width=width, height=height)
      heatmap.sig.genes(isolate(DE$result), meta.method=isolate(input$meta.method),
                        fdr.cut=isolate(input$fdr.cut), color="GR")
      dev.off()
    }, session)
    output$heatmap <- renderImage(
      list(src=outfile, contentType='image/png', alt="heatmap",
           width=width, height=height)
    , deleteFile=TRUE)
    done(session)
  })
  
  observeEvent(input$runpath, {	
    wait(session, "running pathway analysis, should be soon")
    try({
      if(is.null(DE$result)) stop("You need to run DE analysis first")
      meta.p  <- DE$result$meta.analysis$pval
      pathway <- getOption(input)$pathway
      enrichment <- getOption(input)$enrichment
      p.cut <- getOption(input)$p.cut      
      DEgene.number <- getOption(input)$DEgene.number
      size.min <- getOption(input)$size.min       
      size.max <- getOption(input)$size.max
      DE$pathresult <- PathAnalysis(meta.p=meta.p,pathway=pathway,
                              enrichment=enrichment,p.cut=p.cut,
                              DEgene.number=DEgene.number,
                              size.min=size.min, size.max=size.max)           
      dir.path <- paste(DB.load.working.dir(db), "Meta DE", sep="/")
      if (!file.exists(dir.path)) dir.create(dir.path)
      file.path <- paste(dir.path, "pathway.summary.csv", sep="/")
      write.csv(DE$pathresult, file=file.path)
      sendSuccessMessage(session, paste("pathway summary file written to", file.path))
    }, session)
    done(session)
  })    
###  
  
  ##########################
  # Render output/UI       #
  ##########################
          
  output$summaryTable <- renderTable({
        if(!is.null(DB$active)){
            table <- matrix(NA, length(DB$active@datasets), 2 )
            colnames(table) <- c("#Genes","#Samples")
            rownames(table) <- names(DB$transpose)
            for (i in 1:length(DB$transpose)){
                table[i,2] <- dim(DB$transpose[[i]])[1]
                table[i,1] <- dim(DB$transpose[[i]])[2]
            }
            return(table)
        }
    })
  
  output$meta.type.opt <- renderUI({
    meta.type <- input$meta.type
    if (meta.type == META.TYPE.p) {
      tagList(
        uiOutput(ns("meta.p.opt")),
        uiOutput(ns("meta.method.opt"))
      )
    } else if (meta.type == META.TYPE.effect) {
      tagList(
        selectizeInput(ns("meta.method"), "Meta Method", as.list(META.effect.all)),
        uiOutput(ns("meta.method.opt"))
      )
    } else if (meta.type == META.TYPE.other) {
      selectizeInput(ns("meta.method"), "Meta Method", as.list(META.other.all))
    }
  })

  output$meta.p.opt <- renderUI({
    if(input$meta.type == META.TYPE.p && length(input$advanced.method) > 0) {
      if (input$advanced.method)
        selectizeInput(ns("meta.method"), "Meta Method", as.list(META.p.all))
      else
        selectizeInput(ns("meta.method"), "Meta Method", as.list(META.p.core))
    }
  })

  output$meta.method.opt <- renderUI({
    method <- input$meta.method
    advanced <- input$advanced.method
    if (length(method) > 0 && length(advanced) > 0 && advanced == T) {
      if (method == META.roP || method == META.roP.OC) {
        n <- length(DB$active@datasets)
        sliderInput(ns("rth"), "rth", min=1, max=n, value=1, step=1)
      } else if (method == META.AW) {
        selectInput(ns("AW.type"), "AW Type", AW.all)
      } else if (method == META.REM) {
        selectInput(ns("REM.type"), "REM Type", REM.all)
      }
    }
  })

  output$ind.method.opt <- renderUI({
    study <- DB$active
    if (!is.null(study)) {
      study.names <- names(study@datasets)
       if (input$meta.type == META.TYPE.p) {
         bsCollapse(bsCollapsePanel("Setting Individual Study Method",
          lapply(seq_along(study.names), function(index) {
            tag.id <- ns(paste("ind", index, sep=""))
            selectizeInput(tag.id, study.names[index], IND.all)
          })
        ),
          bsCollapsePanel("Setting Individual Study Paired Option",
                        lapply(seq_along(study.names), function(index) {
                          tag.id <- ns(paste("paired", index, sep=""))
                          radioButtons(tag.id, paste(study.names[index], "paired?"),
                                       c(Yes=T, No=F), F, inline=T)
                        })
        ))
      } else {
        bsCollapsePanel("Setting Individual Study Paired Option",
                        lapply(seq_along(study.names), function(index) {
                          tag.id <- ns(paste("paired", index, sep=""))
                          radioButtons(tag.id, paste(study.names[index], "paired?"),
                                       c(Yes=T, No=F), F, inline=T)
                        })
        )        
      }
    }
  })

  output$downloadCsv <- downloadHandler(
    filename=function(){"metaDE.result.csv"},
    content=function(file) {
      write.csv(DE$summary, file=file)
    }
  )

  output$resp.type.option <- renderUI({
    try({
      validate()
      resp <- input$resp.type
      clinical.options <- names(DB$active@clinicals[[1]])
      if (resp == RESP.two.class || resp == RESP.multi.class) {
        tagList(
          selectInput(ns("label.col"), "Label Attribute:", clinical.options),
          uiOutput(ns("class.option"))
        )
      } else if (resp == RESP.continuous) {
        selectInput(ns("conti.col"), "Label Attribute:", clinical.options)
      } else if (resp == RESP.survival) {
        tagList(
          selectInput(ns("time.col"), "Time Attribute:", clinical.options),
          selectInput(ns("indicator.col"), "Indicator Attribute:", clinical.options)
        )
      }
    }, session)
  })

  output$class.option <- renderUI({
    try({
      validate()
      labels <- DB$active@clinicals[[1]][,input$label.col]
      labels <- levels(as.factor(labels))
      if (input$resp.type == RESP.two.class) {
        tagList(
          selectInput(ns("control.label"), "Control Label:", labels, selected=labels[1]),
          selectInput(ns("expr.label"), "Experimental Label:", labels, selected=labels[2])
        )
      } else if (input$resp.type == RESP.multi.class) {
        tagList(
          selectizeInput(ns("group.label"), "Group Labels:", labels, 
                         multiple=T, options = select.noDefault),
          selectInput(ns("control.label"), "Control Label:", labels)
        )
      }
    }, session)
  })

  output$summary <- DT::renderDataTable(DT::datatable({
    DE$summary
  }))

  output$geneLeft <- renderText({
    if (!is.null(DE$summary)) {
      count <- sum(DE$summary$FDR <= input$fdr.cut)
      paste(count, "genes left after cut off")
    }
  })

  output$para.opt <- renderUI({
    if (input$parametric == F) {
      numericInput("meta_de-nperm", "number of permutation", value=100)
    }
  })

  output$covariate.opt <- renderUI({
    clinical.options <- names(DB$active@clinicals[[1]])
    selectInput(ns("covariate"), "Covariate:", c(None="None", clinical.options))
  })

  output$plot.opt <- renderUI({
    if (!is.null(DE$result)) {
      tagList(
        fluidRow(
          column(3, numericInput(ns("fdr.cut"), "FDR Cutoff", value=1e-9)),
          column(3, textOutput(ns("geneLeft"), container=div)),
          column(3, sliderInput(ns("scale"), "Image Size", value=1, min=0.5, max=4)),
          column(3, actionButton(ns("plot"), "Plot DE Genes Heatmap",
                      icon=icon("paint-brush"), class="btn-success btn-run lower-btn"))
        ),
        imageOutput(ns('heatmap'), height="100%")
      )
    }
  })

  output$enrichment.opt <- renderUI({
     if (input$enrichment == ENRICHMENT.fisher) {
          tagList(
           numericInput(ns("p.cut"), "p-value cutoff", NULL),
           textOutput(ns("geneLeft.path"), container=div),
           numericInput(ns("DEgene.number"), "number of DE genes", NULL)
          )
    }
  })
  
  output$geneLeft.path <- renderText({
    if (!is.null(DE$summary)) {
      count <- sum(DE$summary$pval <= input$p.cut)
      paste(count, "genes are selected for pathway analysis \n")
    }
  })
  

  output$downloadPathwayCsv <- downloadHandler(
    filename=function(){"pathway.result.csv"},
    content=function(file) {
      write.csv(DE$pathresult, file=file)
    }
  )

  output$pathresult <- DT::renderDataTable(DT::datatable({
    DE$pathresult
  }))
  
}
