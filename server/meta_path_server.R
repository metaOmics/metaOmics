meta_path_server <- function(input, output, session) {
  library(MetaPath)
  data(pathways)

  ns <- NS("meta_path")

  ##########################
  # Reactive Values        #
  ##########################
  DB <- reactiveValues(active=DB.load.active(db))
  DE <- reactiveValues(result=NULL)

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
    tryCatch({
      file.path <- paste(DB.load.working.dir(db), "Meta DE", "result.rds", sep="/")
      DE$result <- readRDS(file.path)
      sendInfoMessage(session, MSG.detect.de.result)
    })
  })

  output$srcSelect <- renderUI({
    if (!is.null(DE$result)) {
      radioButtons(ns("useDE"), 'Use Meta DE Result:', inline=T,
        c(Yes=T, No=F), T
      )
    }
  })


  ##########################
  # Render output/UI       #
  ##########################
}
