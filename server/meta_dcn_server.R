meta_dcn_server <- function(input, output, session){
  ns <- NS("meta_dcn")
  
  library(MetaDCN)
  data(pathwayDatabase)

  getOption <- function(input) {
   study <- DB$active
   n <- length(study@datasets)
   opt <- list(data=NULL, labels=NULL, caseName=NULL, controlName=NULL,
    meanFilter=0, SDFilter=0, FDRCutoff=NULL, edgeCutoff=NULL, 
    MCSteps=500, jaccardCutoff=0.8, permutationTimes=NULL, repeatTimes=NULL, 
    outputFigure=TRUE, outputPrefix="MetaDCN", CPUNumbers=NULL, 
    pathwayDatabase=pathwayDatabase, silent=FALSE)
	 opt$data <- study@datasets
   clinical.options <- names(DB$active@clinicals[[1]])
   opt$labels <- sapply(1:n,function(x) 
    DB$active@clinicals[[x]][,clinical.options[1]])
   opt$caseName <- input$caseName
   opt$controlName <- input$controlName
   opt$FDRCutoff <- input$FDRCutoff
   opt$edgeCutoff <- input$edgeCutoff
   opt$permutationTimes <- input$permutationTimes
   opt$repeatTimes <- input$repeatTimes
   opt$CPUNumbers <- input$CPUNumbers
   return(opt)
  }

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
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

  observeEvent(input$run, {
    wait(session, "running MetaDCN, may take a while")
    try({
      dir.path <- paste(DB.load.working.dir(db), "Meta DCN", sep="")
      if (!file.exists(dir.path)) dir.create(dir.path)
      old.dir <- getwd()
      setwd(dir.path)

      res <- do.call(MetaDCN, getOption(input))
      #load("res2.RData")
      MetaDCNRes$w1 <- res$w1
      MetaDCNRes$ModuleInCase <- res$ModuleInCase
      MetaDCNRes$ModuleInControl <- res$ModuleInControl
      MetaDCNRes$supermodule <- res$supermodule
      sendSuccessMessage(session, paste("MetaDCN files written to", dir.path))
      
      #print(MetaDCNRes$ModuleInCase[input$HModule,"Component.Number"])

      # print High module
      output$HModuleHeader <- renderText({
        "Basic modules higher correlated in case"
        })
      if(nrow(MetaDCNRes$ModuleInCase) > 0){
        output$HModuleText <- renderText({
          paste(nrow(MetaDCNRes$ModuleInCase),
            " modules higher correlated in case under FDR ", 
            getOption(input)$FDR, ", select modules to plot:", sep="")
          })
        output$HModuleSelect <- renderUI({
            selectInput(ns('HModule'), '', 
              seq(1,nrow(MetaDCNRes$ModuleInCase)))
          })
        output$HModuleTable <- DT::renderDataTable(DT::datatable({
          res$ModuleInCase
          }))
      }else{
        output$HModuleText <- renderText({
        paste("No modules higher correlated in case under FDR ", 
          getOption(input)$FDR, sep="")
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
            getOption(input)$FDR, ", select modules to plot:", sep="")
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
            getOption(input)$FDR, sep="")
        })
      }

      # Print super module
      output$supermodule <- DT::renderDataTable(DT::datatable({res$supermodule
      }))

      setwd(old.dir)

    }, session)
    done(session)
  })

  # Plot High modules
  observeEvent(input$HModule, {
    output$HMImage <- renderImage({
      img.src <- paste(DB.load.working.dir(db), 
        "Meta DCN/MetaDCN_figure_weight_", MetaDCNRes$w1, 
        "_forward_component_", 
        MetaDCNRes$ModuleInCase[as.numeric(input$HModule), "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
        "Repeat.Index"],
         ".png", sep="")
      list(src=img.src, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src <- renderText({
      paste("File name:", DB.load.working.dir(db), 
        "Meta DCN/MetaDCN_figure_weight_", MetaDCNRes$w1, 
              "_forward_component_", 
              MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Component.Number"],
              "_repeat_", MetaDCNRes$ModuleInCase[as.numeric(input$HModule), 
              "Repeat.Index"],
               ".png", sep="")
      })  
  })

  # Plot Low modules
  observeEvent(input$LModule, {
    output$LMImage <- renderImage({
      img.src2 <- paste(DB.load.working.dir(db), 
        "Meta DCN/MetaDCN_figure_weight_", MetaDCNRes$w1, 
        "_backward_component_", 
        MetaDCNRes$ModuleInControl[as.numeric(input$LModule), 
        "Component.Number"],
        "_repeat_", MetaDCNRes$ModuleInControl[as.numeric(input$LModule), 
        "Repeat.Index"],
         ".png", sep="")
      list(src=img.src2, contentType='image/png', alt="module")
    }, deleteFile = FALSE)
    output$src2 <- renderText({
      paste("File name: ", DB.load.working.dir(db), 
        "Meta DCN/MetaDCN_figure_weight_", MetaDCNRes$w1, 
              "_backward_component_", 
              MetaDCNRes$ModuleInControl[as.numeric(input$LModule), "Component.Number"],
              "_repeat_", MetaDCNRes$ModuleInControl[as.numeric(input$LModule), 
              "Repeat.Index"],
               ".png", sep="")
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