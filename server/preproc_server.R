preproc_server <- function(input, output, session) {

  DB <- reactiveValues(names=DB.ls(db))

  observeEvent(DB$names, {
    updateSelectizeInput(session, "dataset", choices=DB$names,
      selected=input$studyName)
  })

  observeEvent(input$tabChange, {
    DB$names <- DB.ls(db)
  })

  validate.data <- function(data) {
    if(class(data) != "data.frame") stop(MSG.datasetInput.typeerror)
    if(all(dim(data) == 0)) stop(MSG.datasetInput.noinput)
    if(dim(data)[1] == 0) stop(MSG.datasetInput.norow)
    if(dim(data)[2] == 0) stop(MSG.datasetInput.nocol)
  }

  validate.study <- function(study) {
    if((input$dataset == "") && (input$log == T) && (ntype(study) == "discrete"))
      stop(MSG.study.nolog)
    name <- study@name
    if(is.null(name) || name == "") stop(MSG.study.noname)
    if(name %in% DB.ls(db)) stop(MSG.study.duplicate(name))
  }

  ##########################
  # Choosing / Upload Data #
  ##########################
  datasetInput <- reactive({
    dataset <- NULL
    if (input$dataset == "") {
      inFile <- input$file
      if (!is.null(inFile)) {
        dataset <- ReadExpr(inFile$datapath, header=input$header,
          sep=input$sep, quote=input$quote, log=input$log)
        updateTextInput(session, "studyName", value=inFile$name)
      }
    } else {
      study <- DB.load(db, input$dataset)
      updateTextInput(session, "studyName", value=study@name)
      updateSelectizeInput(session, "dtype", selected=study@dtype)
      updateSelectizeInput(session, "ntype", selected=study@ntype)
      updateSelectizeInput(session, "stype", selected=study@stype)
      dataset <- as.matrix(study)
    }
    itype <- input$id.type
    if (itype  != id.type[["Gene Symbol"]]) {
      ip <- input$platform
      is <- input$species
      if (itype == id.type[["Probe ID"]] && !is.null(ip) && ip != "") {
        dataset <- Annotate(dataset, id.type=itype, platform=input$platform)
      } else if (!is.null(is) && is != "") {
        dataset <- Annotate(dataset, id.type=itype, species=input$species)
      }
    }
    as.data.frame(dataset)
  })
  
  output$studyName <- renderText({
    input$studyName
  })

  output$summary <- renderPrint({
    dataset <- datasetInput()
    if (is.null(dataset))
      "No file uploaded"
    else
      summary(dataset )
  })

  output$dataPreview <- DT::renderDataTable(DT::datatable({
    datasetInput()
  }))

  ##########################
  # Annotation             #
  ##########################
  data(platform.info)
  platform.options <- as.list(platform.info[,1])
  names(platform.options) <- platform.info[,1]
  selectPlatform <- selectizeInput("preproc-platform", "Platform:", platform.options, 
    options = select.noDefault
  )
  selectSpecies <- selectizeInput("preproc-species", "Species:", species.option,
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
        validate.data(datasetInput())
        study <- new("Study",
          name=input$studyName,
          dtype=input$dtype,
          datasets=list(as.matrix(datasetInput()))
        )
        validate.study(study)
        DB.save(db, study, file=input$studyName)
        sendSuccessMessage(session, paste("Study", study@name, "saved."))
        DB$names <- DB.ls(db)
      },
      warning=function(w) {sendWarningMessage(session, w$message)},
      error=function(e) {sendErrorMessage(session, e$message)}
    )
  })
}
