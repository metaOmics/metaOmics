installed <- installed.packages()[,"Package"]
enabled <- c()
for (toolset in TOOLSET.all) {
  if (toolset %in% installed) {
    enabled <- c(enabled, toolset)
  }
}

toolsets <- c("Toolsets")
if (TOOLSET.de %in% enabled)
  toolsets <- c(toolsets, list(meta_de_ui("meta_de")))
if (TOOLSET.clust %in% enabled)
  toolsets <- c(toolsets, list(meta_clust_ui("meta_clust")))

if (length(toolsets) > 1) {
  toolsets <- do.call(navbarMenu, toolsets)
} else {
  toolsets <- tags$div()
}

shinyUI(
  navbarPage("metaOmics", id="nav",
    header=tagList(
      tags$div(id="working-dir",
        tagList(
          tags$p("Working Directory", class="header-label"),
          tags$p(textOutput("setting-working.dir"))
        )
      ),
      tags$div(id="active-study", 
        tagList(
          tags$p("Active Study", class="header-label"),
          tags$div(tags$p(textOutput("saved_data-activated")))
        )
      )
    ),
    # tab for global settings
    setting_ui("setting"),
    # tab for preprosessing
    preproc_ui("preproc"),
    # tab for manipulating saved data
    saved_data_ui("saved_data"),
    # tab for toolsets
    toolsets,
    tags$div(
      tags$div(id="loading",
        tags$div(id="loadingcontent",
          tags$p(id="loadingspinner", "loading......")
        )
      )
    ),
    # Including css and javascripts in head section
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/messenger-theme-future.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$script(src="js/spin.min.js"),
      tags$script(src="js/messenger.min.js"),
      tags$script(src="js/message-handler.js")
    )
  )
)
