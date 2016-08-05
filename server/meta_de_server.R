meta_de_server <- function(input, output, session) {

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))

  ##########################
  # Validation             #
  ##########################

  ##########################
  # Observers              #
  ##########################
  observeEvent(input$tabChange, {
    DB$active <- DB.load.active(db)
  })

  expand.btn <- actionButton('meta_de-expand.advanced.opt',
                             'Show advanced options', icon=icon("plus"))
  close.btn <- actionButton('meta_de-close.advanced.opt',
                            'Hide advanced options', icon=icon("minus"))
  observeEvent(input$expand.advanced.opt, {
    group.choices <- colnames(DB$active@clinicals[[1]])
    output$toggle.option <- renderUI({
      tagList(
        close.btn,
        selectizeInput("meta_de-group", "select group", group.choices, multiple=T),
        numericInput("meta_de-nperm", "number of permutation", NULL),
        selectizeInput("meta_de-tail", "tail", TAIL.all)
      )
    })
  })
  observeEvent(input$close.advanced.opt, {
    output$toggle.option <- renderUI({
      expand.btn
    })
  })

  ##########################
  # Render output/UI       #
  ##########################
  output$ind.method <- renderUI({
    study <- DB$active
    if (!is.null(study)) {
      study.names <- names(study@datasets)
      index <- 0
      lapply(study.names, function(study.name) {
        index <- index + 1
        tag.id <- paste("meta_de-", index, sep="")
        selectizeInput(tag.id, study.name, as.list(IND.all))
      })
    } else {
      "You haven't select an active study yet"
    }
  })

  output$toggle.option <- renderUI({ expand.btn })
}
