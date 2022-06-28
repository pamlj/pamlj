# Here we put very general functions, usefull over and beyond this package

ginfo <- function(what = NULL, obj = NULL) {
    if (j_INFO) {
        if (!is.null(what)) 
            print(what)
        if (!is.null(obj)) {
            print(obj)
            cat("------------\n")
        }
    }
}

mark <- function(what = NULL, obj = NULL) {
    if (j_DEBUG) {
        if (!is.null(what)) 
            print(what) else print("you got here")

        if (!is.null(obj)) {
            print(obj)
            print("#### end ###")
        }
    }
}



is.something <- function(x, ...) UseMethod(".is.something")

.is.something.default <- function(obj) (!is.null(obj))

.is.something.list <- function(obj) (length(obj) > 0)

.is.something.numeric <- function(obj) (length(obj) > 0)

.is.something.character <- function(obj) (length(obj) > 0)

.is.something.logical <- function(obj) !is.na(obj)



