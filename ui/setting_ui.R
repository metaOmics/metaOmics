setting_ui <- function(id, label = "global settings") {
  ns <- NS(id)
  
  tabPanel("Settings", value=id,
    h1("Welcome to MetaOmics",align = "center",style="primary"),
    tags$hr(),
    fluidRow(
      img(src='pic.png',align="left",height = '280px', width = '380px'),
      p("MetaOmics is an interactive software with graphical user interface (GUI) for genomic meta-analysis implemented using R shiny. Many state of art meta analysis tools are available in this software, including MetaQC for quality control, MetaDE for differential expression analysis, MetaPath for pathway enrichment analysis, MetaNetwork for differential co-expression network analysis, MetaPredict for classification analysis, MetaClust for sparse clustering analysis, MetaPCA for principal component analysis."), 
      p("Our tool is available for download on github: ",a(strong("MetaOmics."), href="https://github.com/metaOmics/metaOmics",target="_blank"), "For detailed implementation of each tool, please refer to our ",a(strong("Tutorials."), href="https://github.com/metaOmics/tutorial/
blob/master/metaOmics_turtorial.pdf",target="_blank")), 
      p("MetaOmics is developed and maintained by ", a("Dr. George Tseng's group ",href="http://tsenglab.biostat.pitt.edu",target="_blank"),"from the Department of Biostatistics, University of Pittsburgh."),
      style="text-indent: 25px; font-size: 20px"),
      tags$hr(),
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
          tags$tr(tags$td("MetaQC"), tags$td(uiOutput(ns("opt.MetaQC")))),
          tags$tr(tags$td("MetaDE"), tags$td(uiOutput(ns("opt.MetaDE")))),
          tags$tr(tags$td("MetaPath"), tags$td(uiOutput(ns("opt.MetaPath")))),
          tags$tr(tags$td("MetaNetwork"), tags$td(uiOutput(ns("opt.MetaDCN")))),
          tags$tr(tags$td("MetaPredict"), tags$td(uiOutput(ns("opt.MetaKTSP")))),          
          tags$tr(tags$td("MetaClust"), tags$td(uiOutput(ns("opt.MetaSparseKmeans")))),          
          tags$tr(tags$td("MetaPCA"), tags$td(uiOutput(ns("opt.metaPCA"))))
        )
      )
    )
  )
}
