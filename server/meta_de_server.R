meta_de_server <- function(input, output, session) {

  ns <- NS("meta_de")

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))

  ##########################
  # Validation             #
  ##########################
  validate <- function() {
    if(length(DB$active) == 0 )
      warning(MSG.no.active)
  }

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
    try({
      validate()
      output$toggle.option <- renderUI({
        tagList(
          close.btn,
          numericInput("meta_de-nperm", "number of permutation", NULL),
          selectizeInput("meta_de-tail", "tail", TAIL.all)
        )
      })
    }, session)
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
    try({
      validate()
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
    }, session)
  })

  observeEvent(input$two.class.col, {
    try({
      validate()
      resp <- input$resp.type
      labels <- DB$active@clinicals[[1]][,input$two.class.col]
      labels <- levels(as.factor(labels))
      output$class.option <- renderUI({
        tagList(
          selectInput(ns("control.label"), "Control Label:", labels),
          selectInput(ns("expr.label"), "Experimental Label:", labels)
        )
      })
    }, session)
  })

  observeEvent(input$multi.class.col, {
    try({
      validate()
      resp <- input$resp.type
      labels <- DB$active@clinicals[[1]][,input$multi.class.col]
      labels <- levels(as.factor(labels))
      output$class.option <- renderUI({
	tagList(
          selectizeInput(ns("group.label"), "Group Labels:", labels, 
                         multiple=T, options = select.noDefault),
          selectInput(ns("control.label"), "Control Label:", labels)
        )
      })
    }, session)
  })

  observeEvent(input$run, {
    study <- DB$active

    ind.method <- unlist(lapply(1:length(study@datasets), function(index) {
      tag.id <- paste("ind", index, sep="")
      input[[tag.id]]
    }))

    resp <- input$resp.type
    response <- ""
    ref.level <- ""
    select.group <- ""
    if (resp == RESP.two.class) {
      response <- input$two.class.col
      select.group <- c(input$control.label, input$expr.label)
      ref.level <- input$control.label
    } else if (resp == RESP.multi.class) {
      response <- input$multi.class.col
      select.group <- input$multi.class.col
      ref.level <- input$control.label
    } else if (resp == RESP.continuous) {
      response <- input$conti.col
    } else if (resp == RESP.survival) {
      response <- c(input$time.col, input$indicator.col)
    }

    cat(file=stderr(), "data: ", class(study@datasets),"\n")
    cat(file=stderr(), "clin.data: ", class(study@clinicals),"\n")
    cat(file=stderr(), "data.type: ", study@dtype,"\n")
    cat(file=stderr(), "resp.type: ", input$resp.type,"\n")
    cat(file=stderr(), "response: ", response,"\n")
    cat(file=stderr(), "meta.method: ", input$meta.method,"\n")
    cat(file=stderr(), "select.group: ", select.group,"\n")
    cat(file=stderr(), "ref.level: ", ref.level,"\n")
    cat(file=stderr(), rep(FALSE,length(study@datasets)), "\n")
    meta.res.p <- MetaDE(
      data=study@datasets,
      clin.data=study@clinicals,
      data.type=study@dtype,
      resp.type=input$resp.type,
      response=response,
      covariate=NULL,
      ind.method=ind.method,
      meta.method=input$meta.method,
      select.group=select.group,
      ref.level=ref.level,
      paired=rep(FALSE,length(study@datasets)),
      rth=NULL,
      AW.type="original",
      REM.type=NULL,
      asymptotic.p=FALSE
    )
    cat(file=stderr(), "3\n")

    output$heatmap <- renderImage({
      meta.method <- 'AW'
      outfile <- tempfile(fileext='.png')
      png(outfile)
      heatmap.sig.genes(meta.res.p, meta.method=meta.method,
                        fdr.cut=1e-7,color="GR")
      list(src=outfile, contentType='image/png', alt="heatmap")
    }, deleteFile=TRUE)
    cat(file=stderr(), "4\n")
  })

  ##########################
  # Render output/UI       #
  ##########################
  output$ind.method <- renderUI({
    try({
      study <- DB$active
      study.names <- names(study@datasets)
      index <- 0
      lapply(study.names, function(study.name) {
        index <- index + 1
        tag.id <- ns(paste("ind", index, sep=""))
        selectizeInput(tag.id, study.name, IND.all)
      })
    }, session)
  })

  output$toggle.option <- renderUI({ expand.btn })

}
