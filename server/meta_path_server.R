meta_path_server <- function(input, output, session) {

  library(MetaDE)	
  library(MetaPath)
  data(pathways)

  ns <- NS("meta_path")

  getOption <- function(input) {
    study <- DB$active
    n <- length(study@datasets)
    opt <- list(ind.method=rep(IND.limma, n), covariate=NULL, rth.value=NULL, select.group=NULL , ref.level=NULL)
     
      opt$arraydata <- DB$active@datasets
      opt$clinical.data <- DB$active@clinicals
      opt$data.type <- DB$active@dtype

    if (length(input$method) > 0) {
      method <- input$method
      opt$method  <- input$method
      if (input$method == MAPE.MAPE) {
	      opt$stat <- input$stat
	    if (input$stat == MAPE.STAT.rth) {
	      opt$rth.value <- input$rth.value
	    }
      }
    } else {
          opt$method <- MAPE.CPI    	
    }      

      opt$ind.method <- unlist(lapply(1:n, function(index) {
        tag.id <- paste("ind", index, sep="")
        if (length(input[[tag.id]]) == 0)
          IND.limma
        else
          input[[tag.id]]
      }))
      
      opt$paired <- unlist(lapply(1:n, function(index) {
        tag.id <- paste("paired", index, sep="")
        if (length(input[[tag.id]]) == 0)
          F
        else
          input[[tag.id]] == T
      }))
      
      if(input[["mixed"]]==T){
       print(MSG.mixed.type.MetaDE)
       opt$mixed <- input[["mixed"]]      
       opt$mix.type <- unlist(lapply(1:n, function(index) {
        tag.id <- paste("type", index, sep="")
        if (length(input[[tag.id]]) == 0)
          IND.continuous
        else
          input[[tag.id]]
       }))     	
      } else{
        opt$mixed <- F
        opt$mix.type <- rep(opt$data.type,n)   
      } 	
            
    if (length(input$covariate) == 0 || input$covariate == "None") {
    } else {
      opt$covariate <- input$covariate
    }
    
    if (length(input$tail) > 0) {
      opt$tail <- input$tail
    }

    resp <- input$resp.type
    clinical.options <- names(DB$active@clinicals[[1]])
    labels <- DB$active@clinicals[[1]][,clinical.options[1]]
    labels <- levels(as.factor(labels))

    opt$resp.type <- resp
    if (resp == RESP.two.class) {
      if (length(input$label.col) > 0) {
        opt$label <- input$label.col
        opt$select.group <- c(input$control.label, input$expr.label)
        opt$ref.level <- input$control.label
      } else {
        opt$label <- clinical.options[1]
        opt$select.group <- c(labels[1], labels[2])
        opt$ref.level <- labels[1]
      }
    } else if (resp == RESP.multi.class) {
      if (length(input$label.col) > 0) {
        opt$label <- input$label.col
        opt$select.group <- input$multi.class.col
        opt$ref.level <- input$control.label
      } else {
        opt$label <- clinical.options[1]
        opt$select.group <- labels
        opt$ref.level <- labels[1]
      }
    } else if (resp == RESP.continuous) {
      if (length(input$label.col) > 0) {
        opt$label <- input$conti.col
      } else {
        opt$label <- clinical.options[1]
      }
    } else if (resp == RESP.survival) {
      if (length(input$time.col) > 0 && length(input$indicator.col)) {
        opt$label <- c(input$time.col, input$indicator.col)
      } else {
        opt$label <- c(clinical.options[1], clinical.options[2])
      }
    }

    opt$pathway <- c()
    for (pathway in input$pathway) {
      opt$pathway <- c(opt$pathway, get(pathway))
    }
    opt$enrichment  <- input$enrichment

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
    dir.path <- paste(DB$working, "clustering diagnostics", sep="/")
    if (!file.exists(dir.path)) dir.create(dir.path)
    opt$output_dir <- dir.path
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
    dir.path <- paste(DB.load.working.dir(db), "MetaPath", sep="")
    if (!file.exists(dir.path)) dir.create(dir.path)
    DB$working <- dir.path
    DB$transpose <- lapply(DB$active@datasets,t)
  })

  observeEvent(input$run, {
    wait(session, "performing Meta Path Analysis")
    try({
      MAPE$result <- do.call(MAPE2.0, getOption(input))
      dir.path <- DB$working
      saveRDS(MAPE$result, file=paste(dir.path, "result.rds", sep="/"))
      write.csv(MAPE$result$summary, file=paste(dir.path, "summary.csv", sep="/"))
      sendSuccessMessage(session,
        paste("result raw data / summary csv file  written to", dir.path))
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
      img.src <- paste(DB$working, "clustering diagnostics", "consensus021.png", sep="/")
      list(src=img.src, contentType='image/png', alt="consensus plot")
    })

    output$delta <- renderImage({
      img.src <- paste(DB$working, "clustering diagnostics", "consensus022.png", sep="/")
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
                    genelist=result$genelist,
                    kappa.result=diagnostics$kappa,
                    pathway=result$pathway,
                    enrichment=result$enrichment,
                    method=diagnostics$method,
                    software=result$method,
                    output_dir=DB$working)
    sendSuccessMessage(session, paste("Clustering result saved to:", DB$working))

    img.src <- paste(DB$working, paste("Clustering_files",input$Num_Clusters,"clusters",sep="_"), "Heatmap_clusters_all.png", sep="/")
    print(img.src)
    output$heatmap <- renderImage(              
     list(src=img.src, contentType='image/png', alt="heatmap of clusters")
     )           
    done(session)
  })
  
  

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


  output$ind.method.opt <- renderUI({
    study <- DB$active
    if (!is.null(study)) {
      study.names <- names(study@datasets)
      bsCollapse(
        bsCollapsePanel("Setting Individual Data Type",
          lapply(seq_along(study.names), function(index) {
            tag.id <- ns(paste("type", index, sep=""))
            selectizeInput(tag.id, study.names[index], IND.type)
          })
        ),
        bsCollapsePanel("Setting Individual Study Method",
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
    }
  })

  output$covariate.opt <- renderUI({
    clinical.options <- names(DB$active@clinicals[[1]])
    selectInput(ns("covariate"), "Covariate:", c(None="None", clinical.options))
  })

  output$method.opt <- renderUI({
      selectizeInput(ns('method'), "Software:", MAPE.all)
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
#        selectizeInput(ns('permutation'), "Permutation Method", PERMUTE.all),
        textOutput(ns("warn"), container=div),
        numericInput(ns("nperm"), "Number of Permutation", 500),
        selectizeInput(ns('qvalue.cal'), "q-value Calculation Method", QVALUE.all)
      )
    }
  })

  output$warn <- renderText({
      paste0("Warning: permutation will be slow \n")
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
        numericInput(ns("Num_Clusters"), "Number Of Clusters", 5, min=2),
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
        left <- sum(MAPE$result$summary[paste(input$kappa.method,"FDR",sep="_")] <= input$q_cutoff)
      } else {
        left <- sum(MAPE$result$summary["q_value_meta"] <= input$q_cutoff)
      }
      paste(left, "pathways left after cutoff")
    }
  })

}
