preproc_server <- function(input, output, session) {

  DB <- reactiveValues(names=DB.ls(db))

  observeEvent(DB$names, {
    updateSelectizeInput(session, "dataset", choices=DB$names,
      selected=input$studyName)
    updateSelectizeInput(session, "id.type", selected=ID.TYPE.geneSymbol)
  })

  observeEvent(input$tabChange, {
    DB$names <- DB.ls(db)
  })

  validate.study <- function(study) {
    dataset <- study@datasets[[1]]
    if(all(dim(dataset) == 0)) stop(MSG.datasetInput.noinput)
    if(dim(dataset)[1] == 0) stop(MSG.datasetInput.norow)
    if(dim(dataset)[2] == 0) stop(MSG.datasetInput.nocol)
    if((input$dataset == "") && (input$log == T) && is.discrete(study))
      stop(MSG.study.nolog)
    name <- study@name
    if(is.null(name) || name == "") stop(MSG.study.noname)
    if(name %in% DB.ls(db)) stop(MSG.study.duplicate(name))
    validObject(study)
  }

  ##########################
  # Choosing / Upload Data #
  ##########################
  studyInput <- reactive({
    study <- NULL
    if (input$dataset == "") {
      inFile <- input$file
      if (!is.null(inFile)) {
        updateTextInput(session, "studyName", value=inFile$name)
        study <- ReadExpr(inFile$datapath, name=inFile$name, dtype=DTYPE.microarray,
          header=input$header, sep=input$sep, quote=input$quote, log=input$log)
      }
    } else {
      study <- DB.load(db, input$dataset)[[1]]
      updateTextInput(session, "studyName", value=study@name)
      updateSelectizeInput(session, "dtype", selected=study@dtype)
      updateSelectizeInput(session, "ntype", selected=study@ntype)
      updateSelectizeInput(session, "stype", selected=study@stype)
    }
    if (input$impute != "none") {
      study <- Impute(study, method=input$impute)
    }
    itype <- input$id.type
    if (!is.null(study) && itype != id.type[["Gene Symbol"]]) {
      ip <- input$platform
      is <- input$species
      if (itype == id.type[["Probe ID"]] && !is.null(ip) && ip != "") {
        study <- Annotate(study, id.type=itype, platform=input$platform)
      } else if (!is.null(is) && is != "") {
        study <- Annotate(study, id.type=itype, species=input$species)
      }
      if(all(is.na(row.names(study@datasets[[1]]))))
        sendWarningMessage(session, MSG.annotate.wrong.platform)
      else if (input$replicate != "none")
        study <- PoolReplicate(study, method=input$replicate)
    }
    study
  })
  
  output$studyName <- renderText({
    input$studyName
  })

  output$summary <- renderPrint({
    study <- studyInput()
    if (is.null(study))
      "No file uploaded"
    else
      summary(study@datasets[[1]])
  })

  output$dataPreview <- DT::renderDataTable(DT::datatable({
    study <- studyInput()
    if (!is.null(study))
      study@datasets[[1]]
  }))

  ##########################
  # Annotation             #
  ##########################
  selectPlatform <- selectizeInput("preproc-platform", "Platform:", as.list(PLATFORM.all), 
    options = select.noDefault
  )
  selectSpecies <- selectizeInput("preproc-species", "Species:", as.list(SPECIES.all),
    options = select.noDefault
  )
  output$id.type.option <- renderUI({
    switch (input$id.type,
      ProbeID=selectPlatform,
      RefSeqID=selectSpecies,
      EntrezID=selectSpecies,
      default=NULL
    )
  })

  ##########################
  # Save and Metadata      #
  ##########################
  observeEvent(input$saveStudy, {
    tryCatch( {
        study <- studyInput()
        study@name  <- input$studyName
        study@dtype <- input$dtype
        validate.study(study)
        DB.save(db, study)
        sendSuccessMessage(session, paste("Study", study@name, "saved."))
        DB$names <- DB.ls(db)
      },
      warning=function(w) {sendWarningMessage(session, w$message)},
      error=function(e) {sendErrorMessage(session, e$message)}
    )
  })
}
