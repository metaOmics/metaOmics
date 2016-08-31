setting_ui <- function(id, label = "global settings") {
  ns <- NS(id)
  tabPanel("Settings", value=id,
    mainPanel(
      h2("Session Information"),
      verbatimTextOutput(ns("urlText")),
      h2("Directory for Saving Output Files:", style="display:inline"),
      helpIcon("working_dir_help", HELP.working.dir),
      directoryInput(ns('directory'), label='select a directory'),
      h2("Toolsets"),
      tags$table(class="table",
        tags$thead(class="thead-default",
          tags$tr(tags$th("Package"), tags$th("Status"))
        ),
        tags$tbody(
          tags$tr(tags$td("Meta DE"), tags$td(uiOutput(ns("opt.MetaDE")))),
          tags$tr(tags$td("Meta Clust"), tags$td(uiOutput(ns("opt.MetaSparseKmeans")))),
          tags$tr(tags$td("Meta Path"), tags$td(uiOutput(ns("opt.MetaPath")))),
          tags$tr(tags$td("Meta PCA"), tags$td(uiOutput(ns("opt.MetaPCA")))),
          tags$tr(tags$td("Meta KTSP"), tags$td(uiOutput(ns("opt.MetaKTSP"))))
        )
      )
    )
  )
}
