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
        updateDirectoryInput(session, 'directory', value = path)
        output$working.dir <- renderText({path})
      }
    }
  )

  ##########################
  # Render output/UI       #
  ##########################
}
