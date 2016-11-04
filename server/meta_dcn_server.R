meta_dcn_server <- function(input, output, session){
  ns <- NS("meta_dcn")
  
  library(MetaDCN)
  data(pathwayDatabase)

  getOption1 <- function(input) {
   study <- DB$active
   dir.path <- paste(DB.load.working.dir(db), "Meta DCN", sep="")
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
         repeatTimes=4, outputFigure=TRUE, silent=FALSE)
   opt$MCSteps <- input$MCSteps
   opt$jaccardCutoff <- input$jaccardCutoff
   opt$repeatTimes <- input$repeatTimes
   return(opt)
  }

  getOption3 <- function(input, GeneNetRes) {
   opt <- list(GeneNetRes, FDRCutoff=0.3, w1=NULL, silent=FALSE)
   opt$FDRCutoff <- input$FDRCutoff
   return(opt)
  }


  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  GeneNetRes <- reactiveValues()
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
  })

  observeEvent(input$GeneNet, {
    wait(session, "Generate network, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "Meta DCN", sep="")
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
          value=4)
        })
      output$MCSteps <- renderUI({
        numericInput(ns("MCSteps"), "MC steps:",
          value=500)
        })
      output$jaccardCutoff <- renderUI({
        numericInput(ns("jaccardCutoff"), "jaccardCutoff",
          value=0.8)
        })
      output$SearchBM <- renderUI({
        actionButton(ns('SearchBM'), 'Search for basic modules', icon=icon("rocket"), class="btn-success btn-run")
        })

    }, session)
    done(session)
  })

  observeEvent(input$SearchBM, {
    wait(session, "Search for basic modules, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "Meta DCN", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)
      do.call(SearchBM, getOption2(input, GeneNetRes))
      sendSuccessMessage(session, paste("Basic modules files written to", 
        dir.path))
      
      # print High module
      output$FDRCutoff <- renderUI({
        sliderInput(ns("FDRCutoff"), label="FDR Cutoff", value=0.3, min=0, max=1)
        })

      output$MetaDCN <- renderUI({
        actionButton(ns('MetaDCN'), 'Assemble supermdules', icon=icon("rocket"), class="btn-success btn-run")
        })

    }, session)
    done(session)
  })

  observeEvent(input$MetaDCN, {
    wait(session, "running MetaDCN, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "Meta DCN", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)

      res2 <- do.call(MetaDCN, getOption3(input, GeneNetRes))
      MetaDCNRes$w1 <- res2$w1
      MetaDCNRes$ModuleInCase <- res2$ModuleInCase
      MetaDCNRes$ModuleInControl <- res2$ModuleInControl
      MetaDCNRes$supermodule <- res2$supermodule
      sendSuccessMessage(session, paste("MetaDCN files written to", dir.path))
      
      # print High module
      output$HModuleHeader <- renderText({
        "Basic modules higher correlated in case"
        })
      if(nrow(MetaDCNRes$ModuleInCase) > 0){
        output$HModuleText <- renderText({
          paste(nrow(MetaDCNRes$ModuleInCase),
            " modules higher correlated in case under FDR ", 
            getOption3(input, GeneNetRes)$FDRCutoff, 
            ", select modules to plot:", sep="")
          })
        output$HModuleSelect <- renderUI({
            selectInput(ns('HModule'), '', 
              seq(1,nrow(MetaDCNRes$ModuleInCase)))
          })
        output$HModuleTable <- DT::renderDataTable(DT::datatable({
          res2$ModuleInCase
          }))
      }else{
        output$HModuleText <- renderText({
        paste("No modules higher correlated in case under FDR ", 
          getOption3(input, GeneNetRes)$FDRCutoff, sep="")
        })
      }
      
      # Print low module
      output$LModuleHeader <- renderText({
        "Basic modules higher correlated in control"
        })
      if(nrow(MetaDCNRes$ModuleInControl) > 0){
        output$LModuleText <- renderText({
          paste(nrow(MetaDCNRes$ModuleInControl), 
            " modules higher correlated in control under FDR ", 
            getOption3(input, GeneNetRes)$FDRCutoff, 
            ", select modules to plot:", sep="")
        })
        output$LModuleSelect <- renderUI({
            selectInput(ns('LModule'), '', 
              seq(1,nrow(MetaDCNRes$ModuleInControl)))
          })
        output$LModuleTable <- DT::renderDataTable(DT::datatable({
          MetaDCNRes$ModuleInControl
          }))
      }else{
        output$LModuleText <- renderText({
          paste("No modules higher correlated in control under FDR ", 
            getOption3(input, GeneNetRes)$FDRCutoff, sep="")
        })
      }

      # Print super module
      output$supermoduleHeader <- renderText({
        "MetaDCN pathway-guided supermodules"
        })
      output$supermodule <- DT::renderDataTable(DT::datatable({res2$supermodule
      }))


    }, session)
    done(session)
  })


  # Plot High modules
  observeEvent(input$HModule, {
    output$HMImage <- renderImage({
      img.src <- paste(DB.load.working.dir(db), 
        "Meta DCN/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_", 
        MetaDCNRes$ModuleInCase[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_forward.png", sep="")
      list(src=img.src, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src <- renderText({
      paste("File name:", DB.load.working.dir(db), 
        "Meta DCN/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$ModuleInCase[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_forward.png", sep="")
      })  
  })

  # Plot Low modules
  observeEvent(input$LModule, {
    output$LMImage <- renderImage({
      img.src2 <- paste(DB.load.working.dir(db), 
        "Meta DCN/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$ModuleInCase[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_backward.png", sep="")
      list(src=img.src2, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src2 <- renderText({
      paste("File name:", DB.load.working.dir(db), 
        "Meta DCN/Basic_modules_figures_weight_", MetaDCNRes$w1, 
        "/Basic_module_component_",
        MetaDCNRes$ModuleInCase[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Repeat.Index"], "_weight_", MetaDCNRes$w1, 
              "_backward.png", sep="")
      })  
  })

  ##########################
  # Render output/UI       #
  ##########################
  output$caseName = renderUI({
      selectInput(ns('caseName'), 'Case Name', 
        unique(DB$active@clinicals[[1]][,1]),
       selected=unique(DB$active@clinicals[[1]][,1])[1])
  })
  output$controlName = renderUI({
      selectInput(ns('controlName'), 'Control Name', 
        unique(DB$active@clinicals[[1]][,1]), 
        selected=unique(DB$active@clinicals[[1]][,1])[2])
  })
  output$CPUNumbersButton = renderUI({
      selectInput(ns('CPUNumbers'), 'Number of CPUs', 
        seq(1,input$permutationTimes), 
        selected=1)
  })

}