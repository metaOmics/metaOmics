preproc_server <- function(input, output, session) {

  datasets <- reactiveValues(names=dataset_names())

  observeEvent(datasets$names, {
    updateSelectizeInput(session, "dataset", choices=datasets$names,
      selected=input$studyName)
  })

  observeEvent(input$tabChange, {
    datasets$names <- dataset_names()
  })

  ##########################
  # Choosing / Upload Data #
  ##########################
  datasetInput <- reactive({
    if (input$dataset == "") {
      inFile <- input$file
      if (is.null(inFile)) {
        NULL
      } else {
        dataset <- ReadExpr(inFile$datapath, header=input$header,
          sep=input$sep, quote=input$quote, log=input$log)
        updateTextInput(session, "studyName", value=inFile$name)
        as.data.frame(dataset)
      }
    } else {
      study <- load_study(input$dataset)
      updateTextInput(session, "studyName", value=study@name)
      updateSelectizeInput(session, "dtype", selected=study@dtype)
      updateSelectizeInput(session, "ntype", selected=study@ntype)
      updateSelectizeInput(session, "stype", selected=study@stype)
      as.data.frame(study@dataset)
    }
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
  platform.options <- as.list(paste(platform.info[,2], platform.info[,3], sep="::"))
  names(platform.options) <- platform.info[,1]
  selectPlatform <- selectizeInput("platform", "Platform:", platform.options, 
    options = select.noDefault
  )
  selectSpecies <- selectizeInput("species", "Species:", species.option,
    options = select.noDefault
  )
  output$ID.type.option <- renderUI({
    switch (input$ID.type,
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
    if (!is.null(datasetInput())) {
      study <- new("Study",
        name=input$studyName,
        ntype=input$ntype,
        stype=study.stype[["single study"]],
        dtype=input$dtype,
        dataset=as.matrix(datasetInput())
      )
      saveRDS(study, file=paste(dataset.dir, input$studyName, sep="/"))
      session$sendCustomMessage(type = 'simpleAlert',
        message = paste("study saved:", input$studyName)
      )
      datasets$names <- dataset_names()
    } else {
      session$sendCustomMessage(type = 'simpleAlert',
        message = "No valid data yet"
      )
    }
  })

}
