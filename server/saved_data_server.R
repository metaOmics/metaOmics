saved_data_server <- function(input, output, session) {

  datasets <- reactiveValues(
    names=dataset_names(),
    meta=dataset_meta(),
    studies=load_studies()
  )

  observeEvent(input$tabChange, {
    datasets$names <- dataset_names()
  })

  observe({
    datasets$names
    datasets$meta <- dataset_meta()
    datasets$studies <- load_studies()
  })

  output$table <- DT::renderDataTable(DT::datatable({
    datasets$meta
  }))

  output$selected <- renderText({
    selected <- input$table_rows_selected
    if(length(selected) == 0)
      "You haven't select anything yet"
    else
      paste(selected, sep=", ")
    
  })

  observeEvent(input$delete, {
    unlink(paste(dataset.dir, input$table_rows_selected, sep="/"))
    session$sendCustomMessage(type = 'simpleAlert',
      message = paste("deleted:", input$table_rows_selected)
    )
    datasets$names <- dataset_names()
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
      types <- datasets$meta[selected,"numeric nature"]
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
    studies <- datasets$studies[selected]
    studies <- Merge(lapply(studies, function(s) as.matrix(s)),
                     lapply(studies, function(s) s@dtype))
    study <- new("Study",
      name=input$studyName,
      dtype=input$dtype,
      datasets=studies
    )
    saveRDS(study, file=paste(dataset.dir, input$studyName, sep="/"))
    session$sendCustomMessage(type = 'simpleAlert',
      message = paste("study saved:", input$studyName)
    )
    datasets$names <- dataset_names()
  })
}
