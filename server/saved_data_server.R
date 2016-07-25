saved_data_server <- function(input, output, session) {

  datasets <- reactiveValues(names=dataset_names())

  observeEvent(input$tabChange, {
    datasets$names <- dataset_names()
  })

  output$table <- DT::renderDataTable(DT::datatable({
    datasets$names
    studies <- load_studies()
    cat(file=stderr(), "1\n")
    if(length(studies) > 0) {
      metas <- lapply(studies, function(x) {
        nrows <- nrow(x@datasets[[1]])
        ncols <- 0
        for(d in x@datasets) {
          ncols <- ncols + ncol(d)
        }
        c(x@dtype, x@ntype, x@stype, nrows, ncols)
      })
      n <- length(metas)
      metas <- unlist(metas)
      dim(metas) <- c(5, n)
      metas <- t(metas)
      colnames(metas) <- c("data type", "numeric nature", "study type", 
                           "features", "sample size")
      rownames(metas) <- unlist(lapply(studies, function(x) x@name))
      as.data.frame(metas)
    }
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
}
