`%dofuture%` <- doFuture::`%dofuture%`



paml_palette <- function(what = NULL) {
    p <- function(...) viridis::viridis(..., alpha = .7)

    switch(what,
        viridis = p <- function(...) viridis::viridis(..., alpha = .7),
        turbo = p <- function(...) viridis::turbo(..., alpha = .7),
        rocket = p <- function(...) viridis::rocket(..., alpha = .7),
        mako = p <- function(...) viridis::mako(..., alpha = .7),
        cividis = p <- function(...) viridis::cividis(..., alpha = .7),
        plasma = p <- function(...) viridis::plasma(..., alpha = .7),
        husky = p <- function(...) {
            mark(list(...))
            grDevices::grey.colors(...)
        }
    )
    return(p)
}

fields_tothem <- function(obj, ...) UseMethod(".fields_tothem")


.fields_tothem.correlation <- function(obj, names) {
    names[names == "alpha"] <- "sig.level"
    names[names == "es"] <- "r"
    return(names)
}

fields_tome <- function(obj, ...) UseMethod(".fields_tome")


.fields_tome.correlation <- function(obj, names) {
    names[names == "sig.level"] <- "alpha"
    names[names == "r"] <- "es"
    return(names)
}


nicify_param <- function(what, short = FALSE) {
    if (short) {
        name <- switch(what,
            n = "N",
            es = "ES",
            power = "Power",
            sig.level = "Alpha"
        )
        return(name)
    }
    switch(what,
        n = "Sample size (N)",
        es = "Effect size",
        power = "Power",
        sig.level = "Type I error rate (alpha)"
    )
}



link_help <- function(obj, ...) UseMethod(".link_help")

.link_help.default <- function(obj) {
    text <- NULL
    link <- LINKS[[obj$mode]]

    if (is.null(link)) link <- LINKS[[obj$caller]]

    if (is.something(link)) {
        text <- "<p style='display: flex; align-items: center;'> " %+%
            "<span style=' display:inline-block; text-align: center;" %+%
            "width:16px; height:16px; border: 3px solid green; border-radius: 50%;padding:3px; padding-bottom:3px; margin-right:8px;" %+%
            "font-weight: bolder'>" %+%
            "i</span><span> Help can be found <a href='" %+% link %+% "' target='_blank'> Pamlj web manual.</a> <span></p>"
    }
    return(text)
}
