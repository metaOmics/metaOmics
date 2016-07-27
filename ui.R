shinyUI(
  navbarPage("metaOmics", id="nav",
    header=h5(textOutput("saved_data-activated"), id="active-study"),
    # tab for preprosessing
    preproc_ui("preproc"),
    # tab for manipulating saved data
    saved_data_ui("saved_data"),
    #tab for metaClust
   meta_clust_ui("meta_clust"),
    # Including css and javascripts in head section
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger-theme-future.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$script(src="js/messenger.min.js"),
      tags$script(src="js/message-handler.js")
    )
  )
)
