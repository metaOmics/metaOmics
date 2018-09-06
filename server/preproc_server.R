preproc_server <- function(input, output, session) {

  ns <- NS("preproc")
  ##########################
  # Reactive Values        #
  ##########################
  DB   <- reactiveValues(names=DB.ls(db))
  STUDY <- reactiveValues(action="", update=0, ori=NULL, preview=NULL, clinicals=NULL)
  GEO <- reactiveValues(name=NULL,expr=NULL,pheno=NULL) ##

  ##########################
  # Validation             #
  ##########################
  # Other than Study's validation, we add application specific validation here
  validate.study <- function(study) {
    dataset <- study@datasets[[1]]
    if(all(dim(dataset) == 0)) stop(MSG.datasetInput.noinput)
    if(dim(dataset)[1] == 0) stop(MSG.datasetInput.norow)
    if(dim(dataset)[2] == 0) stop(MSG.datasetInput.nocol)
    if((input$study == "") && (input$log == T) && ntype(study) == NTYPE.discrete)
      stop(MSG.study.nolog)
    name <- study@name
    if(is.null(name) || name == "") stop(MSG.study.noname)
    if(name %in% DB.ls(db)) stop(MSG.study.duplicate(name))
    validObject(study)
  }

  ##########################
  # Observers              #
  ##########################
  # watch for tab change, get the newest list of all data
  observeEvent(input$tabChange, {DB$names <- DB.ls(db)}, 
               label="tab change")  
               
#  # retrieve GEO dataset
#  observeEvent(input$retrieveGEOData, {
#  	if (!is.null(input$geo)) {
#    wait(session, "Retrieving GEO data, should be soon")
#    try({
#    	    number <- length(GEO)
#        gset <- getGEO(input$geo, GSEMatrix =TRUE,getGPL=F,destdir = tempdir())
#        gset <- gset[[1]]
#        geo.expr <- exprs(gset)    	
#        geo.pheno <- pData(phenoData(gset))
#    	    GEO[[number +1]] <- geo.expr
#    	    names(GEO)[number+1] <- input$geo
      
#     sendSuccessMessage(session, paste(input$geo," data retrieved from GEO database"), unique=T)
#               
#     }, session)
#    }
#    done(session)
#  }) ##
  
  # download geo data 
  observeEvent(input$downloadGEOData, {
  	if (!is.null(input$geo)) {
    wait(session, "Downloading GEO data, should be soon")
    try({
        gset <- getGEO(input$geo, GSEMatrix =TRUE,getGPL=F,destdir = tempdir())
        gset <- gset[[1]]
        geo.expr <- exprs(gset)    	
        geo.pheno <- pData(phenoData(gset))
        GEO$name <- input$geo   	        	   
    	    GEO$expr <- geo.expr
    	    GEO$pheno <- geo.pheno
                
      geo.dir.path <- paste(DB.load.working.dir(db), "GEO", sep="/")
      if (!file.exists(geo.dir.path)) dir.create(geo.dir.path)
      
      write.csv(geo.expr, file=paste(geo.dir.path,"/",input$geo,"_expr.csv",sep=""))
      write.csv(geo.pheno, file=paste(geo.dir.path,"/",input$geo,"_pheno.csv",sep=""))
      
     sendSuccessMessage(session, paste(input$geo," data retrieved from GEO database and downloaded to local directory"), unique=T)
     
     }, session)
    }
    done(session)  	
  }) ##
  
#  observe({
#  	 updateSelectizeInput(session, 'study', server = T, choices = names(GEO),selected=tail(names(GEO),1))
#  })
                                            
  # watch for expression file upload  
  observeEvent(c(input$header, input$data.sep, input$data.quote,
                 input$log, input$exprfile), {
    if (!is.null(input$exprfile)) {
      STUDY$action <- STUDY.expression.upload
      STUDY$update <- STUDY$update + 1
      updateSelectizeInput(session, "study", selected="")
      STUDY$clinicals <- NULL
    }
  }, label="expression file upload")  
  # watch for clinical file upload
  observeEvent(c(input$clinical.sep, input$clinical.quote, input$clinical), {
    if (!is.null(input$clinical)) {
      STUDY$action <- STUDY.clinical.upload
      STUDY$update <- STUDY$update + 1
    }
  }, label="clinical file upload")
  # watch for GEO study selection
  observeEvent(c(input$useGEO, input$logGEO), {
    if (input$useGEO ==T){
    	    if(input$logGEO==T) {
          try({	
      	   dat <- isolate(GEO$expr)
      	   dat[dat <= 0] <- 0.001
      	   GEO$expr <- log2(dat) 
         },session)
        }    	  
      STUDY$action <- STUDY.select.from.geo
      STUDY$update <- STUDY$update + 1
    }
  }, label="use GEO study") ##
  # watch for study selection
  observeEvent(input$study, {
    if (input$study %in% DB$names) {
      STUDY$action <- STUDY.select.from.db
      STUDY$update <- STUDY$update + 1
      session$sendCustomMessage(type="resetFile", message="#preproc-exprfile")
      session$sendCustomMessage(type="resetFile", message="#preproc-clinical")
    }
  }, label="select study from database") 
  
  # Setting study from file type
  observeEvent(STUDY$update, {
    if (STUDY$action == STUDY.expression.upload) {
      wait(session, "Parsing Expression File...")
      inFile <- input$exprfile
      tryCatch({
        STUDY$ori <- ReadExpr(inFile$datapath, name=inFile$name, dtype=DTYPE.microarray,
          header=input$header, sep=input$data.sep, quote=input$data.quote, log=input$log)
      }, error=function(error) {
        sendErrorMessage(session, error$message)
      })
      done(session)
    } else if (STUDY$action == STUDY.select.from.geo) {
      tryCatch({STUDY$ori <- new("Study",
                        name= isolate(GEO$name),
                        dtype=DTYPE.microarray, 
                        datasets = list(GEO$expr), 
                        clinicals = list(GEO$pheno));
                STUDY$clinicals <- list(GEO$pheno)        
                    }, error=function(error) {
        sendErrorMessage(session, error$message)
      }) ##  
    } else if (STUDY$action == STUDY.select.from.db) {
      STUDY$ori <- DB.load(db, input$study)[[1]]
      STUDY$clinicals <- STUDY$ori@clinicals
    } else if (STUDY$action == STUDY.clinical.upload) {
      wait(session, "Parsing Clinical File...")
      inFile <- input$clinical
      tryCatch({
        clinical <- ReadClinical(inFile$datapath, sep=input$clinical.sep, 
                                       quote=input$clinical.quote)
        STUDY$clinicals <- list(clinical)
      }, error=function(error) {
        sendErrorMessage(session, MSG.file.corrupted)
      })
      done(session)
    }
  }, label="setting STUDY$ori from file upload or selection")

  # watch for change in STUDY$preview to update meta data
  observeEvent(STUDY$preview, {
    study <- STUDY$preview
    updateTextInput(session, "studyName", value=study@name)
    updateSelectizeInput(session, "dtype", selected=study@dtype)
    updateSelectizeInput(session, "ntype", selected=study@ntype)
    updateSelectizeInput(session, "stype", selected=study@stype)
  }, label="updating input option from selected study")

  # processing after read
  observe({
    study <- STUDY$ori
    try({
      if(!is.null(study)) {
        # annotation
        id.type <- input$id.type
        if(id.type != ID.TYPE.geneSymbol) {
          wait(session, "Annotating.... (annotation map is automatically downloaded)")
          platform <- input$platform
          species  <- input$species
          if (id.type == ID.TYPE.probeID && length(platform) > 0 && platform != "")
            study <- Annotate(study, id.type=id.type, platform=input$platform)
          else if (length(species) > 0 && species != "")
            study <- Annotate(study, id.type=id.type, species=input$species)
          done(session)
        }
        # impute
        missing.count <- sum(unlist(lapply(study@datasets, function(x) sum(is.na(x)))))
        if (missing.count == 0) {
          output$impute.opt <- renderUI({
            tags$p("Congratulations! There is no missing values :)")
          })
        } else {
          if (length(input$impute) == 0) {
            output$impute.opt <- renderUI({
              selectInput(ns("impute"), "Method:", IMPUTE.method.all)
            })
          } else {
            wait(session, "Impute Missing Value....")
            study <- Impute(study, method=input$impute)
            done(session)
          }
        }
        # handle replicate
        gene.symbols <- rownames(study@datasets[[1]])
        if (length(unique(gene.symbols)) == length(gene.symbols)) {
          output$replicate.opt <- renderUI({
            tags$p("Congratulations! There is no replicated gene symbols :)")
          })
        } else {
          if (length(input$replicate) == 0) {
            output$replicate.opt <- renderUI({
              selectInput(ns("replicate"), "Method:", REPLICATE.all)
            })
          } else {
            if(all(is.na(row.names(study@datasets[[1]]))))
              sendWarningMessage(session, MSG.annotate.wrong.platform)
            else {
              wait(session, "Handing Replicate Gene Symbol....")
              study <- PoolReplicate(study, method=input$replicate)
              done(session)
            }
          }
        }
        # update preview
        STUDY$preview <- study
      }
    }, session)
  }, label="processing after read")
  
  # Save and Metadata
  observeEvent(input$saveStudy, {
    try({
      if (is.null(STUDY$preview))
        stop(MSG.datasetInput.noinput)       
      study <- STUDY$preview
      study@name  <- input$studyName
      study@dtype <- input$dtype
      study@ntype <- ntype(input$dtype)
      if(stype(study) == STYPE.single) {
        names(study@datasets) <- study@name
      }
      validate.study(study)
      if (!is.null(STUDY$clinicals))
        study <- setClinical(study, STUDY$clinicals)
      DB.save(db, study)
      sendSuccessMessage(session, paste("Study", study@name, "saved."))
      DB$names <- DB.ls(db)
    }, session)
  }, label="save study")

  # watch the list of all study names for change
  observeEvent(DB$names, {
    # update dataset select options, and select the newly saved study
    updateSelectizeInput(session, "study", choices=DB$names,
      selected=input$studyName)
    # update annotation to gene symbol, since saved study shouldn't be annotated
    updateSelectizeInput(session, "id.type", selected=ID.TYPE.geneSymbol)
  }, label="update selected study")
  
  ##########################
  # Render output/UI       #
  ##########################    
  # data title
  output$studyName <- renderText({ paste(input$studyName, "Summary") })
  # summary
  output$summary <- renderPrint({
    study <- STUDY$preview
    if (is.null(study))
      "No file uploaded"
    else
      summary(study@datasets[[1]])
  })
  # expression data preview
  output$dataPreview <- DT::renderDataTable(DT::datatable({
    study <- STUDY$preview
    if (!is.null(study))
      to.matrix(study)
  }))
  # clinical data preview
  output$clinicalPreview <- DT::renderDataTable(DT::datatable({
    clinicals <- STUDY$clinicals
    if (!is.null(clinicals)) {
      clinicals <- lapply(clinicals, function(x) as.matrix(x))
      clinical <- do.call(rbind, clinicals)
      clinical <- cbind("Sample ID"=row.names(clinical), clinical)
      row.names(clinical) <- NULL
      clinical
    }
  }))
  # annotation option
  selectPlatform <- selectizeInput("preproc-platform", "Platform:", 
    PLATFORM.all, options = select.noDefault)
  selectSpecies <- selectizeInput("preproc-species", "Species:", 
    SPECIES.all, options = select.noDefault)
  output$id.type.option <- renderUI({
    switch (input$id.type,
      ProbeID=selectPlatform,
      RefSeqID=selectSpecies,
      EntrezID=selectSpecies,
      default=NULL
    )
  })
}
