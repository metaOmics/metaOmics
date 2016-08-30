meta_de_server <- function(input, output, session) {
  library(MetaDE)

  ns <- NS("meta_de")

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  DE <- reactiveValues(result=NULL, summary=NULL)

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
    DB$working.dir <- DB.load.working.dir(db)
    session$sendCustomMessage(type='initCollapse', message="")
  })

  observeEvent(input$asymptotic.p, {
    output$asym.option <- renderUI({
      if (input$asymptotic.p == F) {
        numericInput("meta_de-nperm", "number of permutation", value=100)
      }
    })
  })

  observeEvent(input$meta.method, {
    method <- input$meta.method
    output$meta.method.option <- renderUI({
      if (method == META.roP || method == META.roP.OC) {
        n <- length(DB$active@datasets)
        sliderInput(ns("rth"), "rth", min=1, max=n, value=1, step=1)
      } else if (method == META.AW) {
        selectInput(ns("AW.type"), "AW Type", AW.all)
      } else if (method == META.REM) {
        selectInput(ns("REM.type"), "REM Type", REM.all)
      }
    })
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
      response <- input$label.col
      select.group <- c(input$control.label, input$expr.label)
      ref.level <- input$control.label
    } else if (resp == RESP.multi.class) {
      response <- input$label.col
      select.group <- input$multi.class.col
      ref.level <- input$control.label
    } else if (resp == RESP.continuous) {
      response <- input$conti.col
    } else if (resp == RESP.survival) {
      response <- c(input$time.col, input$indicator.col)
    }

    asymptotic.p <- input$asymptotic.p
    if (length(asymptotic.p) == 0) asymptotic.p <- F
    tail <- input$tail
    if (length(tail) == 0) tail <- "abs"
    nperm <- input$nperm
    if (length(nperm) == 0) nperm <- 100

    wait(session, "running meta DE, should be soon")
    try({
      DE$result <- MetaDE(
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
        rth=input$rth,
        AW.type=input$AW.type,
        REM.type=input$REM.type,
        asymptotic.p=asymptotic.p,
        nperm=nperm,
        tail=tail
      )
      cat(file=stderr(), "1\n")
      DE$summary <- summary.meta(DE$result, isolate(input$meta.method))
      cat(file=stderr(), "2\n")
      dir.path <- paste(DB.load.working.dir(db), "Meta DE", sep="/")
      if (!file.exists(dir.path)) dir.create(dir.path)
      file.path <- paste(dir.path, "summary.csv", sep="/")
      write.csv(DE$summary, file=file.path)
      sendSuccessMessage(session, paste("summary file written to", file.path))
      file.path <- paste(dir.path, "result.rds", sep="/")
      saveRDS(DE$result, file=file.path)
      sendSuccessMessage(session, paste("raw data written to", file.path), unique=T)
    }, session)
    done(session)
  })

  output$summary <- DT::renderDataTable(DT::datatable({
    DE$summary
  }))

  output$heatmap.info <- renderText({
    count <- sum(DE$summary$FDR <= input$fdr.cut)
    if (!is.null(DE$result)) {
      tail <- input$tail
      if (length(tail) == 0) tail <- "abs"
      paste(isolate(input$meta.method), "with", tail, "alternative hypothesis,",
            count, "significant genes left after cut off.")
    }
  })

  observe({
    if (!is.null(DE$result)) {
      output$heatmap <- renderImage({
        outfile <- tempfile(fileext='.png')
        height <- 800 * input$scale
        width <- 400 * length(DB$active@datasets) * input$scale
        wait(session, paste("Plotting result with FDR",
          input$fdr.cut, "and scale to", input$scale))
        try({
          png(outfile, res=120, width=width, height=height)
          heatmap.sig.genes(isolate(DE$result), meta.method=isolate(input$meta.method),
                            fdr.cut=isolate(input$fdr.cut), color="GR")
          dev.off()
        }, session)
        done(session)
        list(src=outfile, contentType='image/png', alt="heatmap",
             width=width, height=height)
      }, deleteFile=TRUE)
    }
  })

  ##########################
  # Render output/UI       #
  ##########################
  output$ind.method <- renderUI({
    try({
      study <- DB$active
      study.names <- names(study@datasets)
      lapply(seq_along(study.names), function(index) {
        tag.id <- ns(paste("ind", index, sep=""))
        selectizeInput(tag.id, study.names[index], IND.all)
      })
    }, session)
  })

  output$downloadCsv <- downloadHandler(
    filename=function(){"metaDE.result.csv"},
    content=function(file) {
      write.csv(DE$summary, file=file)
    }
  )

  output$resp.type.option <- renderUI({
    try({
      validate()
      resp <- input$resp.type
      clinical.options <- names(DB$active@clinicals[[1]])
      if (resp == RESP.two.class || resp == RESP.multi.class) {
        tagList(
          selectInput(ns("label.col"), "Label Attribute:", clinical.options),
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
    }, session)
  })

  output$class.option <- renderUI({
    try({
      validate()
      labels <- DB$active@clinicals[[1]][,input$label.col]
      labels <- levels(as.factor(labels))
      if (input$resp.type == RESP.two.class) {
        tagList(
          selectInput(ns("control.label"), "Control Label:", labels, selected=labels[1]),
          selectInput(ns("expr.label"), "Experimental Label:", labels, selected=labels[2])
        )
      } else if (input$resp.type == RESP.multi.class) {
        tagList(
          selectizeInput(ns("group.label"), "Group Labels:", labels, 
                         multiple=T, options = select.noDefault),
          selectInput(ns("control.label"), "Control Label:", labels)
        )
      }
    }, session)
  })
}
