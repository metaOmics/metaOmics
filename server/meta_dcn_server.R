meta_dcn_server <- function(input, output, session){
  ns <- NS("meta_dcn")
  
  library(MetaDCN)
  data(pathwayDatabase)

  getOption1 <- function(input) {
   study <- DB$active
   dir.path <- paste(DB.load.working.dir(db), "MetaNetwork", sep="")
   n <- length(study@datasets)
   opt <- list(data=NULL, labels=NULL, caseName=NULL, controlName=NULL,
    meanFilter=0, SDFilter=0, edgeCutoff=NULL, permutationTimes=NULL,
    CPUNumbers=NULL, folder=NULL, 
    pathwayDatabase=pathwayDatabase, silent=FALSE)
	 opt$data <- study@datasets
   clinical.options <- names(DB$active@clinicals[[1]])
   opt$labels <- sapply(1:n,function(x) 
    DB$active@clinicals[[x]][,clinical.options[1]])
   opt$caseName <- input$caseName
   opt$controlName <- input$controlName
   opt$edgeCutoff <- input$edgeCutoff
   opt$CPUNumbers <- input$CPUNumbers
   opt$folder <- dir.path
   opt$permutationTimes <- input$permutationTimes
   return(opt)
  }

  getOption2 <- function(input, GeneNetRes) {
   opt <- list(GeneNetRes, MCSteps=500, jaccardCutoff=0.8, 
         repeatTimes=3, outputFigure=TRUE, silent=FALSE)

   if(length(input$MCSteps) >0)     
   opt$MCSteps <- input$MCSteps
   
   if(length(input$jaccardCutoff) >0)     
   opt$jaccardCutoff <- input$jaccardCutoff
   
   if(length(input$repeatTimes) >0)     
   opt$repeatTimes <- input$repeatTimes
   
   return(opt)
  }

  getOption3 <- function(input, GeneNetRes, SearchBMRes) {
   opt <- list(GeneNetRes, SearchBMRes, FDRCutoff=0.3, w1=NULL, silent=FALSE)
   opt$FDRCutoff <- input$FDRCutoff
   return(opt)
  }


  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  GeneNetRes <- reactiveValues()
  SearchBMRes <- reactiveValues()
  MetaDCNRes <- reactiveValues()

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

  observeEvent(input$GeneNet, {
    wait(session, "Generate network, may take a while")
    try({

      dir.path <- paste(DB.load.working.dir(db), "MetaNetwork", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)
      res <- do.call(GeneNet, getOption1(input))
      GeneNetRes$caseName <- res$caseName
      GeneNetRes$controlName <- res$controlName
      GeneNetRes$permutationTimes <- res$permutationTimes
      GeneNetRes$folder <- dir.path
      GeneNetRes$pathwayDatabase <- res$pathwayDatabase
      GeneNetRes$CPUNumbers <- res$CPUNumbers
      
      sendSuccessMessage(session, paste("Network files written to", dir.path))
      
      # print High module
      output$repeatTimes <- renderUI({
        numericInput(ns("repeatTimes"), "Number to repeat:",
          value=3)
        })
      output$MCSteps <- renderUI({
        numericInput(ns("MCSteps"), "MC steps:",
          value=500)
        })
      output$jaccardCutoff <- renderUI({
        numericInput(ns("jaccardCutoff"), "Jaccard Cutoff",
          value=0.8)
        })
      output$SearchBM <- renderUI({
        actionButton(ns('SearchBM'), 'Search for basic modules', 
          icon=icon("rocket"), class="btn-success btn-run")
        })

    }, session)
    done(session)
  })

  observeEvent(input$SearchBM, {
    wait(session, "Search for basic modules, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "MetaNetwork", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)
      res2 <- do.call(SearchBM, getOption2(input, GeneNetRes))
      SearchBMRes$w1 <- res2$w1
      SearchBMRes$BMInCase <- res2$BMInCase
      SearchBMRes$BMInControl <- res2$BMInControl

      sendSuccessMessage(session, paste("Basic modules files written to", 
        dir.path))
      
      # print BMInCase
      if(nrow(SearchBMRes$BMInCase) > 0){
        output$BMInCaseHeader <- renderText({
          "Basic modules higher correlated in case: "
        })
        output$BMInCaseTable <- DT::renderDataTable(DT::datatable({
          SearchBMRes$BMInCase
          }))
      }else{
        output$BMInCaseHeader <- renderText({
          "No basic modules higher correlated in case"
        })
      }
      
     # print BMInControl
      if(nrow(SearchBMRes$BMInControl) > 0){
        output$BMInControlHeader <- renderText({
          "Basic modules higher correlated in control: "
        })
        output$BMInControlTable <- DT::renderDataTable(DT::datatable({
          SearchBMRes$BMInControl
          }))
      }else{
        output$BMInControlHeader <- renderText({
        "No basic modules higher correlated in control"
        })
      }

      # select FDR
      output$FDRCutoff <- renderUI({
        sliderInput(ns("FDRCutoff"), label="FDR Cutoff", value=0.3, min=0, max=1)
        })

      output$MetaDCN <- renderUI({
        actionButton(ns('MetaDCN'), 'Assemble supermodules', icon=icon("rocket"), class="btn-success btn-run")
        })
    }, session)
    done(session)
  })

  observeEvent(input$MetaDCN, {
    wait(session, "running MetaDCN, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "MetaNetwork", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)

      res3 <- do.call(MetaDCN, getOption3(input, GeneNetRes, SearchBMRes))
      FDRCutoff <- getOption3(input, GeneNetRes, SearchBMRes)$FDRCutoff
      MetaDCNRes$w1 <- res3$w1
      MetaDCNRes$supermodule <- res3$supermodule
      MetaDCNRes$BMInCaseSig <- res3$BMInCaseSig
      MetaDCNRes$BMInControlSig <- res3$BMInControlSig

      sendSuccessMessage(session, paste("MetaDCN files written to", dir.path))
      
      # print High module
      if(nrow(MetaDCNRes$BMInCaseSig) > 0){
        output$BMInCaseText <- renderText({
          paste(nrow(MetaDCNRes$BMInCaseSig),
            " modules higher correlated in case under FDR ", 
            FDRCutoff, 
            ", select modules to plot:", sep="")
          })
        output$BMInCaseSelect <- renderUI({
            selectInput(ns('HModule'), '', 
              seq(1,nrow(MetaDCNRes$BMInCaseSig)))
          })
      }else{
        output$BMInCaseText <- renderText({
        paste("No modules higher correlated in case under FDR ", 
          FDRCutoff, sep="")
        })
      }
      
      # Print low module
      if(nrow(MetaDCNRes$BMInControlSig) > 0){
        output$BMInControlText <- renderText({
          paste(nrow(MetaDCNRes$BMInControlSig), 
            " modules higher correlated in control under FDR ", 
            FDRCutoff, 
            ", select modules to plot:", sep="")
        })
        output$BMInControlSelect <- renderUI({
            selectInput(ns('LModule'), '', 
              seq(1,nrow(MetaDCNRes$BMInControlSig)))
          })
      }else{
        output$BMInControlText <- renderText({
          paste("No modules higher correlated in control under FDR ", 
            FDRCutoff, sep="")
        })
      }

      # Print super module
      output$supermoduleHeader <- renderText({
        "MetaDCN pathway-guided supermodules"
        })
      output$supermodule <- DT::renderDataTable(DT::datatable({MetaDCNRes$supermodule
      }))
    }, session)
    done(session)
  })


  # Plot High modules
  observeEvent(input$HModule, {
    output$HMImage <- renderImage({
      img.src <- paste(DB.load.working.dir(db), 
        "MetaNetwork/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_", 
        MetaDCNRes$BMInCaseSig[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$BMInCaseSig[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_forward.png", sep="")
      list(src=img.src, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src <- renderText({
      paste("File name:", DB.load.working.dir(db), 
        "MetaNetwork/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$BMInCaseSig[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$BMInCaseSig[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_forward.png", sep="")
      })  
  })

  # Plot Low modules
  observeEvent(input$LModule, {
    output$LMImage <- renderImage({
      img.src2 <- paste(DB.load.working.dir(db), 
        "MetaNetwork/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$BMInControlSig[as.numeric(input$LModule), 
        "Component.Number"],
        "_repeat_", MetaDCNRes$BMInControlSig[as.numeric(input$LModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_backward.png", sep="")
      list(src=img.src2, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src2 <- renderText({
      paste("File name:", DB.load.working.dir(db), 
        "MetaNetwork/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$BMInControlSig[as.numeric(input$LModule), 
        "Component.Number"],
        "_repeat_", MetaDCNRes$BMInControlSig[as.numeric(input$LModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_backward.png", sep="")
      })  
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
  
  output$caseName = renderUI({
      selectInput(ns('caseName'), 'Case Name', 
        as.character(unique(DB$active@clinicals[[1]][,1])),
       selected=as.character(unique(DB$active@clinicals[[1]][,1])[1]))
  })

  output$controlName = renderUI({
      selectInput(ns('controlName'), 'Control Name', 
        as.character(unique(DB$active@clinicals[[1]][,1])), 
        selected=as.character(unique(DB$active@clinicals[[1]][,1])[2]))
  })

  output$CPUNumbersButton = renderUI({
      selectInput(ns('CPUNumbers'), 'Number of CPUs', 
        seq(1,input$permutationTimes), 
        selected=1)
  })

}