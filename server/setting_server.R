setting_server <- function(input, output, session) {

  ns <- NS("setting")

  # The check and install module for package
  check.pkg <- function(pkg, label, supported=T, cran.dep=NULL, bioconductor.dep=NULL) {
    active.id <- paste("activate", pkg, sep=".")
    install.id <- paste("install", pkg, sep=".")
    output.id <- paste("opt", pkg, sep=".")
    log.id <- paste("log", pkg, sep=".")
    output[[output.id]] <- renderUI({
    	
       if (TOOLSET.de %in% PACKAGES$installed ==F && TOOLSET.path %in% PACKAGES$installed)
      PACKAGES$installed <- PACKAGES$installed[-grep(TOOLSET.path,PACKAGES$installed,fixed=T)]
	
      PACKAGES$installed
      if (supported) {
        if (installed(pkg)) {
          # radioButtons(ns(active.id), paste(label, 'is currently installed'),
          #              inline=T, c(Activate=T, Disable=F), T)
	  tagList(icon("check-square"), span("installed"))
        } else {
          tagList(
            span(paste(label, "is not installed:")),
            actionButton(ns(install.id), "install", 
			 icon=icon("download"), class="btn-info")
          )
        }
      } else {
        span("We are sorry, this pakage is currently not supported")
      }
    })

    observeEvent(input[[install.id]], {
      try({
        for (package in cran.dep) {
          if(!(package %in% PACKAGES$installed)) {
            sendInfoMessage(session, paste("installing", package, "from CRAN"))
            install.packages(package, repos='http://cran.us.r-project.org')
          }
        }
        for(package in bioconductor.dep) {
          if(!(package %in% PACKAGES$installed)) {
            sendInfoMessage(session, paste("installing", package, "from Bioconductor"))
            biocLite(package, ask=F, suppressUpdates=T, suppressAutoUpdate=T)
          }
        }
        sendInfoMessage(session,paste("installing", pkg, "from Github"))
        devtools::install_github(paste("metaOmic", pkg, sep="/"),force=T)
        
        PACKAGES$installed <- installed.packages()[,"Package"]
        
         if (TOOLSET.de %in% PACKAGES$installed ==F && TOOLSET.path %in% PACKAGES$installed)
        PACKAGES$installed <- PACKAGES$installed[-grep(TOOLSET.path,PACKAGES$installed,fixed=T)]
        
        sendSuccessMessage(session, paste(pkg, "installed"))
        sendSuccessMessage(session, MSG.installed, unique=T)
      }, session)
    })
  }

  ##########################
  # Reactive Values        #
  ##########################
  PACKAGES <- reactiveValues(installed=installed.packages()[,"Package"])

  installed <- function(pkg) {
    pkg %in% PACKAGES$installed
  }

  ##########################
  # Validation             #
  ##########################

  ##########################
  # Observers              #
  ##########################
  observeEvent(input$tabChange, {
    PACKAGES$installed <- installed.packages()[,"Package"]
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
	if(is.na(path)) {
	  sendErrorMessage(session, MSG.no.working.dir)
	} else {
          updateDirectoryInput(session, 'directory', value = path)
          DB.set.working.dir(db, path)
          output$working.dir <- renderText({DB.load.working.dir(db)})
        }
      }
    }
  )

  ##########################
  # Render output/UI       #
  ##########################
  # working directory
  output$working.dir <- renderText({
    try({ DB.load.working.dir(db) }, session)
  })
  # session information
  output$urlText <- renderText({
    server.type <- ""
    if (session$clientData$url_hostname == "127.0.0.1")
      server.type <- "local"
    else
      server.type <- "remote"
    paste(sep = "",
      "protocol: ", session$clientData$url_protocol, "\n",
      "hostname: ", session$clientData$url_hostname, "\n",
      "port: ",     session$clientData$url_port,     "\n",
      "server type: ", server.type,     "\n"
    )
  })

  check.pkg(TOOLSET.qc, 'MetaQC', 
    cran.dep=c("survival", "combinat"),
    bioconductor.dep=c("multtest","survival")
  )

  check.pkg(TOOLSET.de, 'MetaDE', 
    cran.dep=c("survival", "combinat"),
    bioconductor.dep=c("limma", "cluster", "samr", "edgeR", "DESeq2", "impute", "Biobase")
  )
  check.pkg(TOOLSET.path, 'MetaPath',
	    cran.dep=c("gplots", "ggplot2", "shape"),
	    bioconductor.dep=c("Biobase", "GSEABase", "genefilter", "impute", "ConsensusClusterPlus", "irr", "cluster", "AnnotationDbi", "Rgraphviz"))
  check.pkg(TOOLSET.dcn, 'MetaNetwork', cran.dep=c("igraph", "snow", "snowfall"), 
    bioconductor.dep=c("genefilter"))
  check.pkg(TOOLSET.ktsp, 'MetaPredict', cran.dep=c("doMC"))    
  check.pkg(TOOLSET.clust, 'MetaClust', cran.dep=c("cluster"))
  check.pkg(TOOLSET.pca, 'MetaPCA', cran.dep=c("PMA"),bioconductor.dep=c("impute"))

}
