sendWarningMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'warningMessage', message=msg)
}

sendErrorMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'errorMessage', message=msg)
}

sendSuccessMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'successMessage', message=msg)
}

wait <- function(session, msg) {
  session$sendCustomMessage(type = 'wait', message=msg)
}

done <- function(session) {
  session$sendCustomMessage(type = 'done', message="loading...")
}

helpIcon <- function(id, msg) {
  tagList(
    htmlOutput(id, container=tags$i, class="fa fa-question help"),
    bsTooltip(id, msg, "right", trigger = "hover", option=list(html=T))
  )
}

try <- function(code, session) {
  tryCatch(code,
    warning=function(w) {sendWarningMessage(session, w$message)},
    error=function(e) {sendErrorMessage(session, e$message)}
  )
}
