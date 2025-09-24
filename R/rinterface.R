
#'  S3 methods for class jamovi ResultsElement 
#'
#' These functions extract all visible tables from a ResultsElement or related classes produced by pamlj
#' and print them in R style.

#' @param object a pamlj results object of the class `pamlj`
#' @param ... additional arguments passed to the pamlj estimation function
#' @return a list of table as data.frame
#' @author Marcello Gallucci
#' @examples
#' obj<-pamlj::pamlcorr(es=.1)
#' summary(obj)
#' @rdname s3methods
#' 
#' @export

summary.ResultsElement<-function(object,...) {
    
    .get_table<-function(obj) {
        
        if ("Table" %in% class(obj) && obj$visible) {
            if(nrow(obj$asDF)>0) {
                atab<-obj$asDF
                attr(atab,"name")<-obj$name
                attr(atab,"title")<-obj$title
                class(atab)<-c("jmvrtable","data.frame")
                tables[[length(tables)+1]]<<-atab
            }
            return()
        }      
        if ("Html" %in% class(obj) && obj$visible) {
            tables[[length(tables)+1]]<<-obj$content
        }

        if (obj$.has("items")) {
            for (item in obj$items)
                .get_table(item)
        }
        
    }
    
    tables<-list()
    .get_table(object)
    class(tables)<-c("jmvrobj","list")
    tables
}


#'  S3 methods for class palmj_list 
#'
#' These functions extract all visible tables from a list of tables produced by pamlj3
#' and print them in R style.

#' @param object a pamlj results object of the class `pamlj`
#' @param ... additional arguments passed to the pamlj3 estimation function
#' @return a list of tables as data.frame
#' @author Marcello Gallucci
#' @examples
#' data(fivegroups)
#' fivegroups$Group<-factor(fivegroups$Group)
#' gmod<-pamlj3::pamlj_lm(
#'   formula = Score ~Group,
#'   data = fivegroups)
#' 
#' summary(gmod)
#' @rdname s3methods
#' 
#' @export

summary.pamlj_list<-function(object,...) {
 
    lapply(object, function(x) summary.ResultsElement(x))
  
}
    

#' Print a jamovi Table in R style 
#'
#' @param x a pamlj results object of the class `pamlj`
#' @param ... options passed to print.data.frame()

#' @rdname s3methods
#' 
#' @export

print.jmvrtable<-function(x,...) {
    cat(attr(x,"title"),"\n\n")
  for (var in names(x)) {
    if (is.character(x[[var]]))
      x[is.na(x[[var]]),var]<-""  
  }
  print.data.frame(x)
}

#' Print a jamovi summary of jamovi ResultElement in R style 
#'
#' @param x a pamlj results object of the class `pamlj`
#' @param ... options passed to print()
#' @rdname s3methods
#' @export

print.jmvrobj<-function(x,...) {
    for (t in x){
        print(t)
        cat("\n\n")
    }
}

#' Print a jamovi summary of jamovi ResultElement in R style 
#'
#' @param x a pamlj results object of the class `pamlj`
#' @param ... options passed to print()
#' @rdname s3methods
#' @export

print.pamlj_list<-function(x,...) {
    .names<-names(x)
    for (i in seq_along(x)) {
        name<-.names[i]
        cat("Table for ",name,"\n")
         print(x[[i]])
        cat("\n\n")
    }
}

#'  pamlj plots
#'
#' This function extract plots from a pamlj object and return them as a list
#' 
#' @param x a pamlj results object of the class `pamlj`
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' obj<-pamlj::pamlcorr(es=.1,.interface="R",plot_contour=T,plot_x="n",plot_y="es",plot_x_from=10,plot_x_to=100)
#' results<-plots(obj)
#' lapply(results,class)
#' @rdname plot
#' @export

plots <- function(x, ...) {
  
  results<-list()
  if ("powerContour" %in% names(x))
    results$powerContour=x$powerContour$plot
  if ("powerNcurve" %in% names(x))
    results$powerContour=x$powerContour$plot
  if ("powerEscurve" %in% names(x))
    results$powerEscurve=x$powerEscurve$plot
  if ("powerNcurve" %in% names(x))
    results$powerNcurve=x$powerNcurve$plot
  if ("powerCustom" %in% names(x))
    results$powerCustom=x$powerCustom$plot$fun()
  
   if (length(results)==0) return(NULL)
  
  return(results)

}

#'  pamlj plot
#'
#' This function extract plots from a pamlj object. 
#' 
#' @param x a pamlj results object of the class `pamlj`
#' @return an object of class ggplot or a list of ggplot objects
#' @author Marcello Gallucci
#' @examples
#' \dontrun{
#' obj<-pamlj::pamlcorr(es=.1,.interface="R",plot_contour=T,plot_x="n",plot_y="es",plot_x_from=10,plot_x_to=100)
#' plot(obj)
#' }
#' @rdname plot
#' @export

plot.pamlj <- function(x, ...) {
  
  .plots<-plots(x)
  lapply(.plots,print)
  invisible(.plots)
  
  
}


