sendWarningMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'warningMessage', message=msg)
}

sendErrorMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'errorMessage', message=msg)
}

sendSuccessMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'successMessage', message=msg)
}

helpIcon <- function(id, msg) {
  tagList(
    htmlOutput(id, container=tags$i, class="fa fa-question help"),
    bsTooltip(id, msg, "right", trigger = "hover", option=list(html=T))
  )
}
