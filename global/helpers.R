sendWarningMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type = 'warningMessage', list(message=msg, unique=unique))
}

sendErrorMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type='errorMessage', list(message=msg, unique=unique))
}

sendSuccessMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type = 'successMessage', list(message=msg, unique=unique))
}

sendInfoMessage <- function(session, msg, unique=F) {
  session$sendCustomMessage(type = 'infoMessage', list(message=msg, unique=unique))
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
    warning=function(w) {
      sendWarningMessage(session, w$message)
      done(session)
    },
    error=function(e) {
      sendErrorMessage(session, e$message)
      done(session)
    }
  )
}

Try <- try

NS <- function(prefix) { function(name) {paste(prefix, name, sep="-")} }
