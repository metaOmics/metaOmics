saved_data_server <- function(input, output, session) {

  ns <- NS("saved_data")
  active.study <- DB.load.active(db)
  if (is.null(active.study))
    active.study <- "No active study"
  else
    active.study <- active.study@name

  DB <- reactiveValues(meta=meta(db), active=active.study)

  validate.merge.option <- function(input) {
    selected <- input$table_rows_selected
    types <- DB$meta[selected,"numeric nature"]
    if (all(is.continuous(types))) {
      if (is.na(input$mean))
        stop(MSG.merge.nomean)
      else if (is.na(input$var))
        stop(MSG.merge.novariance)
    } else if (all(is.discrete(types))){
      if (is.na(input$threshold))
        stop(MSG.merge.nothreshold)
    } else {
      stop(MSG.merge.mixedtype)
    }

    if(is.null(input$studyName) || input$studyName == "")
      stop(MSG.merge.noname)
    if(input$studyName %in% DB.ls(db))
      stop(MSG.study.duplicate(input$studyName))
  }

  observeEvent(input$tabChange, {
    DB$meta <- meta(db)
  })

  output$table <- DT::renderDataTable(DT::datatable({
    DB$meta
  }))

  output$selected <- renderText({
    selected <- input$table_rows_selected
    if(length(selected) == 0)
      "You haven't select anything yet"
    else
      paste(rownames(meta(db)[selected,]), sep=", ")
    
  })

  observeEvent(input$delete, {
    selected <- input$table_rows_selected
    if(length(selected) == 0) {
      sendErrorMessage(session, "You haven't select anything yet")
    } else {
      selected <- rownames(meta(db)[selected,])
      DB.delete(db, selected)
      sendSuccessMessage(session, paste(selected, "deleted"))
      DB$meta <- meta(db)
      active.study <- DB.load.active(db)
      if (is.null(active.study))
        DB$active <- "No active study"
      else
        DB$active <- active.study@name
    }
  })

  output$activated <- renderText({
    DB$active
  })

  output$merge.option <- renderUI({
    selected <- input$table_rows_selected
    mergeButton <- tagList(
      textInput(ns("studyName"), "Study Name:", ""),
      actionButton(ns("merge"), "Merge from Selected Datasets",
        icon=icon("compress")
      )
    )
    if (length(selected) <= 1) {
      tags$span("You need to select more than one dataset")
    } else {
      types <- DB$meta[selected,"numeric nature"]
      if (all(types == "continuous")) {
        tagList(
          numericInput(ns("mean"), "mean:", value = 0.3, step= 0.1),
          numericInput(ns("var"), "variance:", min = 0, value = 0.3, step= 0.1),
          mergeButton
        )
      } else if (all(types == "discrete")) {
        tagList(
          numericInput(ns("threshold"), "threshold", 1, min=0),
          mergeButton
        )
      } else {
        tags$span(MSG.merge.mixedtype)
      }
    }
  })

  observeEvent(input$merge, {
    tryCatch( {
        validate.merge.option(input)
        selected <- input$table_rows_selected
        selected <- rownames(meta(db)[selected,])
        studies <- DB.load(db, selected)
        study <- Merge(studies, name=input$studyName)
        if (study@ntype == NTYPE.continuous)
          study <- Filter(study, study@dtype, del.perc=c(input$mean, input$var))
        else
          study <- Filter(study, study@dtype, threshold=input$threshold)
        DB.save(db, study)
        message = paste("A merged study is created:", study@name)
        sendSuccessMessage(session, message)
        DB$meta <- meta(db)
      },
      warning=function(w) {sendWarningMessage(session, w$message)},
      error=function(e) {sendErrorMessage(session, e$message)}
    )
  })

  output$active <- renderUI({
    selected <- input$table_rows_selected
    if (length(selected) == 1 ) {
      selected <- rownames(meta(db)[selected,])
      actionButton(ns("active"),
        paste( "Make", selected, "Active Dataset"),
        icon=icon("compress")
      )
    }
  })

  observeEvent(input$active, {
    selected <- input$table_rows_selected
    selected <- rownames(meta(db)[selected,])
    DB.activate(db, selected)
    DB$active <- DB.load.active(db)@name
    message = paste(DB$active, "is now made the active study")
    sendSuccessMessage(session, message)
  })
}
