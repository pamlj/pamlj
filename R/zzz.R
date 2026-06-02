.onLoad <- function(libname, pkgname) {
  op <- options()
  
  op.pamlj <- list(
    pamlj.messages = TRUE
  )
  
  toset <- !(names(op.pamlj) %in% names(op))
  if (any(toset)) {
    options(op.pamlj[toset])
  }
  
  invisible()
}