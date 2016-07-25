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
    dataset <- NULL
    if (input$dataset == "") {
      inFile <- input$file
      if (!is.null(inFile)) {
        dataset <- ReadExpr(inFile$datapath, header=input$header,
          sep=input$sep, quote=input$quote, log=input$log)
        updateTextInput(session, "studyName", value=inFile$name)
      }
    } else {
      study <- load_study(input$dataset)
      updateTextInput(session, "studyName", value=study@name)
      updateSelectizeInput(session, "dtype", selected=study@dtype)
      updateSelectizeInput(session, "ntype", selected=study@ntype)
      updateSelectizeInput(session, "stype", selected=study@stype)
      dataset <- study@dataset
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
