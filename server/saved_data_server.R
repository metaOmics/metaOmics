saved_data_server <- function(input, output, session) {

  DB <- reactiveValues(meta=meta(db), active=DB.load.active(db)@name)

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
      paste(selected, sep=", ")
    
  })

  observeEvent(input$delete, {
    DB.delete(db, input$table_rows_selected)
    session$sendCustomMessage(type = 'simpleAlert',
      message = paste("deleted:", input$table_rows_selected)
    )
    DB$meta <- meta(db)
  })

  output$activated <- renderText({
    DB$active
  })

  output$merge.option <- renderUI({
    selected <- input$table_rows_selected
    mergeButton <- tagList(
      textInput("saved_data-studyName", "Study Name:", ""),
      actionButton("saved_data-merge", "Merge from Selected Datasets",
        icon=icon("compress")
      )
    )
    if (length(selected) <= 1) {
      tags$span("You need to select more than one dataset")
    } else {
      types <- DB$meta[selected,"numeric nature"]
      if (all(types == "continuous")) {
        tagList(
          numericInput("precMean", "mean:", value = 0.3, step= 0.1),
          numericInput("precVar", "variance:", min = 0, value = 0.3, step= 0.1),
          mergeButton
        )
      } else if (all(types == "discrete")) {
        tagList(
          numericInput("threshold", "threshold", 1, min=0),
          mergeButton
        )
      } else {
        tags$span("You can't merge continuous data with discrete data")
      }
    }
  })

  observeEvent(input$merge, {
    selected <- input$table_rows_selected
    studies <- DB.load(db, selected)
    datasets <- Merge(lapply(studies, function(s) as.matrix(s)),
                     lapply(studies, function(s) s@dtype))
    study <- new("Study",
      name=input$studyName,
      dtype=studies[[1]]@ntype,
      datasets=datasets
    )
    DB.save(db, study, input$studyName)
    session$sendCustomMessage(type = 'simpleAlert',
      message = paste("study saved:", input$studyName)
    )
    DB$meta <- meta(db)
  })

  output$active <- renderUI({
    if (length(input$table_rows_selected) == 1 )
      actionButton("saved_data-active",
        paste( "Make", input$table_rows_selected, "Active Dataset"),
        icon=icon("compress")
      )
  })

  observeEvent(input$active, {
    selected <- input$table_rows_selected
    DB.activate(db, selected)
    DB$active <- DB.load.active(db)@name
  })
}
