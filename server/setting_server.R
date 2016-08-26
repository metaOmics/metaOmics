setting_server <- function(input, output, session) {

  ##########################
  # Reactive Values        #
  ##########################

  ##########################
  # Validation             #
  ##########################

  ##########################
  # Observers              #
  ##########################
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
  output$working.dir <- renderText({
    try({ DB.load.working.dir(db) }, session)
  })
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
}
