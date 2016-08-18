meta_de_server <- function(input, output, session) {

  ns <- NS("meta_de")
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

  observeEvent(input$meta.method, {
    method <- input$meta.method
    output$meta.method.option <- renderUI({
      if (method == META.roP || method == META.roP.OC) {
        n <- length(DB$active@datasets)
        sliderInput(ns("rth"), "rth", min=1, max=n, value=1)
      } else if (method == META.AW) {
        selectInput(ns("AW.type"), "AW Type", AW.all)
      } else if (method == META.REM) {
        selectInput(ns("REM.type"), "REM Type", REM.all)
      }
    })
  })

  observeEvent(input$resp.type, {
    resp <- input$resp.type
    clinical.options <- names(DB$active@clinicals[[1]])
    output$resp.type.option <- renderUI({
      if (resp == RESP.two.class) {
        tagList(
          selectInput(ns("two.class.col"), "Label Attribute:", clinical.options),
          uiOutput(ns("class.option"))
        )
      } else if (resp == RESP.multi.class) {
        tagList(
          selectInput(ns("multi.class.col"), "Label Attribute:", clinical.options),
          uiOutput(ns("class.option"))
        )
      } else if (resp == RESP.continuous) {
        selectInput(ns("conti.col"), "Label Attribute:", clinical.options)
      } else if (resp == RESP.survival) {
        tagList(
          selectInput(ns("time.col"), "Time Attribute:", clinical.options),
          selectInput(ns("indicator.col"), "Indicator Attribute:", clinical.options)
        )
      }
    })
  })

  observeEvent(input$two.class.col, {
    resp <- input$resp.type
    labels <- DB$active@clinicals[[1]][,input$two.class.col]
    labels <- levels(as.factor(labels))
    output$class.option <- renderUI({
      tagList(
        selectInput(ns("control.label"), "Control Label:", labels),
        selectInput(ns("expr.label"), "Experimental Label:", labels)
      )
    })
  })

  observeEvent(input$multi.class.col, {
    resp <- input$resp.type
    labels <- DB$active@clinicals[[1]][,input$multi.class.col]
    labels <- levels(as.factor(labels))
    output$class.option <- renderUI({
      selectizeInput(ns("group.label"), "Group Labels:", labels, 
                  multiple=T, options = select.noDefault)
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
        tag.id <- ns(paste("ind", index, sep=""))
        selectizeInput(tag.id, study.name, IND.all)
      })
    } else {
      "You haven't select an active study yet"
    }
  })

  output$toggle.option <- renderUI({ expand.btn })
}
