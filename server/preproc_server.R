preproc_server <- function(input, output, session) {

  ##########################
  # Reactive Values        #
  ##########################
  DB   <- reactiveValues(names=DB.ls(db))
  STUDY <- reactiveValues(action="", update=0, ori=NULL, preview=NULL, clinical=NULL)

  ##########################
  # Validation             #
  ##########################
  # Other than Study's validation, we add application specific validation here
  validate.study <- function(study) {
    dataset <- study@datasets[[1]]
    if(all(dim(dataset) == 0)) stop(MSG.datasetInput.noinput)
    if(dim(dataset)[1] == 0) stop(MSG.datasetInput.norow)
    if(dim(dataset)[2] == 0) stop(MSG.datasetInput.nocol)
    if((input$study == "") && (input$log == T) && is.discrete(study))
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
  # watch for expression file upload
  
  observeEvent(c(input$header, input$data.sep, input$data.quote,
                 input$log, input$exprfile), {
    if (!is.null(input$exprfile)) {
      STUDY$action <- STUDY.expression.upload
      STUDY$update <- STUDY$update + 1
      updateSelectizeInput(session, "study", selected="")
    }
  }, label="expression file upload")
  # watch for clinical file upload
  observeEvent(c(input$clinical.sep, input$clinical.quote, input$clinical), {
    if (!is.null(input$clinical)) {
      STUDY$action <- STUDY.clinical.upload
      STUDY$update <- STUDY$update + 1
    }
  }, label="clinical file upload")
  # watch for study selection
  observeEvent(input$study, {
    if (input$study %in% DB$names) {
      STUDY$action <- STUDY.select.from.db
      STUDY$update <- STUDY$update + 1
      session$sendCustomMessage(type="resetFile", message="#preproc-exprfile")
    }
  }, label="select study from database")

  # Setting study from file type
  observeEvent(STUDY$update, {
    if (STUDY$action == STUDY.expression.upload) {
      inFile <- input$exprfile
      STUDY$ori <- ReadExpr(inFile$datapath, name=inFile$name, dtype=DTYPE.microarray,
        header=input$header, sep=input$data.sep, quote=input$data.quote, log=input$log)
    } else if (STUDY$action == STUDY.select.from.db) {
      STUDY$ori <- DB.load(db, input$study)[[1]]
    } else if (STUDY$action == STUDY.clinical.upload) {
      inFile <- input$clinical
      STUDY$clinical <- ReadClinical(inFile$datapath, sep=input$clinical.sep, 
                                     quote=input$clinical.quote)
    }
  }, label="setting STUDY$ori from file upload or selection")

  # watch for change in STUDY$preview to update meda data
  observeEvent(STUDY$preview, {
    study <- STUDY$preview
    updateTextInput(session, "studyName", value=study@name)
    updateSelectizeInput(session, "dtype", selected=study@dtype)
    updateSelectizeInput(session, "ntype", selected=study@ntype)
    updateSelectizeInput(session, "stype", selected=study@stype)
  }, label="updating input option from selected study")

  # processing after read
  observe({
    # annotation
    study <- STUDY$ori
    if(!is.null(study)) {
      id.type <- input$id.type
      if(id.type != ID.TYPE.geneSymbol) {
        platform <- input$platform
        species  <- input$species
        if (id.type == ID.TYPE.probeID && length(platform) > 0 && platform != "")
          study <- Annotate(study, id.type=id.type, platform=input$platform)
        else if (length(species) > 0 && species != "")
          study <- Annotate(study, id.type=id.type, species=input$species)
      }
      # impute
      if(!is.null(study)) {
        if (input$impute != "none")
          study <- Impute(study, method=input$impute)
      }
      # handle replicate
      if(all(is.na(row.names(study@datasets[[1]]))))
        sendWarningMessage(session, MSG.annotate.wrong.platform)
      else if (input$replicate != "none")
        study <- PoolReplicate(study, method=input$replicate)
      # update preview
      STUDY$preview <- study
    }
  }, label="processing after read")
  
  # Save and Metadata
  observeEvent(input$saveStudy, {
    tryCatch( {
        study <- STUDY$preview
        study@name  <- input$studyName
        study@dtype <- input$dtype
        validate.study(study)
        study <- setClinical(study, list(STUDY$clinical))
        DB.save(db, study)
        sendSuccessMessage(session, paste("Study", study@name, "saved."))
        DB$names <- DB.ls(db)
      },
      warning=function(w) {sendWarningMessage(session, w$message)},
      error=function(e) {sendErrorMessage(session, e$message)}
    )
  }, label="save study")

  # watch the list of all study names for change
  observeEvent(DB$names, {
    # update dataset select options, and select the newly saved study
    updateSelectizeInput(session, "study", choices=DB$names,
      selected=input$studyName)
    # update annotation to gene symbol, since saved study should be annotated
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
      study@datasets[[1]]
  }))
  # clinical data preview
  output$clinicalPreview <- DT::renderDataTable(DT::datatable({
    clinical <- STUDY$clinical
    if (!is.null(clinical))
      clinical
  }))
  # annotation option
  selectPlatform <- selectizeInput("preproc-platform", "Platform:", 
    as.list(PLATFORM.all), options = select.noDefault)
  selectSpecies <- selectizeInput("preproc-species", "Species:", 
    as.list(SPECIES.all), options = select.noDefault)
  output$id.type.option <- renderUI({
    switch (input$id.type,
      ProbeID=selectPlatform,
      RefSeqID=selectSpecies,
      EntrezID=selectSpecies,
      default=NULL
    )
  })
}
