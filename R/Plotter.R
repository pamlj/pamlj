## This class prepares the pathdiagram. It does not produce the actual diagram to avoid
## issues in Windows with semPaths(). The actual diagram is produced in the .rederFun " in .b.R file.

Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Scaffold,
  public=list(

      initialize=function(jmvobj,runner) {
            super$initialize(jmvobj)
            private$.results<-jmvobj$results
            private$.operator<-runner
            private$.results$plotnotes$setContent(" ")
      },

      initPlots=function() {
        ## PAMLj init plots
        private$.initCustom()

      },
      preparePlots=function(image, ggtheme, theme, ...) {
        
            if (!private$.operator$ok) return()
            private$.prepareContour()
            private$.prepareEscurve()
            private$.prepareNcurve()
            private$.prepareCustom()
    
      },
      plot_contour = function(image,ggthem,them) {

        if (!private$.operator$ok) return()
        
        if (!self$option("plot_contour"))
            return()

        data<-image$state
        if (is.null(data)) return()
        

        filled.contour(data$x,data$y,data$z,color.palette =  paml_palette,
               key.title = {mtext("Power",3, .5)},
               ylab =paste("Hypothetical effect size (",data$letter,")", sep = ""),
               xlab="Sample Size (N)",
               plot.axes={
                  axis(1, at=data$ticks, labels=data$tickslabels)
                  axis(2, at=data$yticks, labels=data$ytickslabels)
                  yor<-par()$usr[3]
                  xor<-par()$usr[1]
                  lines(data$x, data$yline, type = "l", lty = 1, lwd=2)
                  segments(xor,data$point.y,data$point.x,data$point.y, lwd=2)
                  segments(data$point.x,yor,data$point.x,data$point.y, lwd=2)
                  points(data$point.x,data$point.y,pch=21,bg="white",cex=1.5)
               })

      },
      plot_curve= function(image,ggtheme,theme) {
         
        
        if (!private$.operator$ok) return()
        if (!self$option("plot_ncurve") && !self$option("plot_escurve"))
                return()

         cols = paml_palette(10)
         state<-image$state
         data <- state$data
         range <- max(data$x)-min(data$x)
         plot(data$x,data$y,  ty='n', 
              xaxt='n',
              ylab=state$ylab, xlab=state$xlab,
              ylim=c(0,1)
              )
         axis(1, at=state$ticks, labels=state$tickslabels)
#        axis(1, at=state$ticks)

         yrect <- seq(0,1,.1)
         yrect[1]<-yrect[1]-.5
         yrect[11]<-yrect[11]+.5
         yor<-par()$usr[3]
         xor<-par()$usr[1]

         for(i in 1:10){
              rect(par()$usr[1], yrect[i], par()$usr[2], yrect[i+1], border = NA,
                   col = cols[i])
         }
         lines(data$x,data$y,lwd=2)
         segments(xor,state$point.y,state$point.x,state$point.y, lwd=2)
         segments(state$point.x,yor,state$point.x,state$point.y, lwd=2)
         points(state$point.x,state$point.y,pch=21,bg="white",cex=1.5)
         mtext(state$text, adj = 1)
       },
      plot_custom= function(image,ggtheme,theme) {
         
        if (!private$.operator$ok) return()

        if (!is.something(image$state)) return()
        
         state<-image$state
         
         data<-state$data
         
         threed<-FALSE


         if (is.something(data$z)) {
           data$z<-factor(data$z)
           threed<-TRUE
         }

         dig=3
         ydif <- max(data$y)-min(data$y)
         xdif <- max(data$y)-min(data$y)

         .nudge<-ggplot2::position_nudge(y = ydif/20)

         if (threed) 
                    .aes <- ggplot2::aes(x=x,y=y,color=z)
         else
                    .aes <- ggplot2::aes(x=x,y=y)
                    
         p <- ggplot2::ggplot(data,.aes)
         p <- p + ggplot2::geom_line( linewidth=1.5)
         p <- p + ggplot2::geom_point(size=2, fill="white", shape=21)
         if (is.something(state$tickdata)) {
                                      if (max(data$y)>9) dig=0
                                      ticks<-unique(state$tickdata$x)
                                      p <- p + ggplot2::geom_label(data=state$tickdata,ggplot2::aes(x=x,y=y,label=round(y,digits=dig)),
                                      position = .nudge, alpha=0,label.size = NA)
                                      p <- p + ggplot2::scale_x_continuous(breaks = ticks)

         }
         p <- p + ggplot2::xlab(state$xlab) + ggplot2::ylab(state$ylab) 
         if (hasName(state,"zlab")) 
             p <- p  + ggplot2::guides(color=ggplot2::guide_legend(title=state$zlab))
        
         p <- p + ggtheme
         return(p)
       }
      
  ), # end of public
  private = list(
    .results=NULL,
    .operator=NULL,
    
    .prepareContour = function() {
      
     if (!self$option("plot_contour"))
              return()
     if (self$option("is_equi"))
              return()

      jinfo("PLOTTER: preparing contour plot")
      
      data <- private$.operator$data
      image<-private$.results$powerContour
      ## check the min-max for effect size
      emax <- private$.operator$data$esmax
      if (emax < data$es) emax<-data$es
      esmin<-  private$.operator$data$esmin

      ## check min-max for N
      
      .data <- data
      .data$es<-ifelse(.data$es*.95 > esmin, .data$es*.95, esmin)
      .data$power<-.98
      .data$n <- NULL
       nmax<-powervector(private$.operator,.data)[["n"]]
      .data <- data
      .data$esmax <- emax
       nmin<-  find_min_n(private$.operator,.data)
   
      if (nmax< data$n) nmax<-data$n+10
      if (nmax<(nmin*2)) nmax=(nmin*2)
      
      point.x<-private$.operator$data$n
      y <- seq(emin,emax,len=20)
      es <- y
      point.y <- data$es
      FLX<-identity
      FEX<-identity
      FLY<-identity
      FEY<-identity

      if (self$option("plot_log")) {
         FLX<-log
         FEX<-exp
         if (private$.operator$loges(data$es)) {
            FLY<-log
            FEY<-exp
         }
      }

       x <- seq(FLX(nmin),FLX(nmax),len=20)
       n <- FEX(x)
       point.x<-FLX(private$.operator$data$n)
       ticks<-seq(FLX(nmin),FLX(nmax),len=6)
       tickslabels<-round(FEX(ticks))
       y  <- seq(FLY(emin),FLY(emax),len=20)
       es <- FEY(y)
       point.y <- FLY(data$es)
       yticks <- seq(FLY(emin),FLY(emax),len=6)
       ytickslabels<-niceround(FEY(yticks))
      .data <- private$.operator$data
      .data$n<-n
      .data$es <- NULL
      mark("yline")
       yline=powervector(private$.operator,.data)[["es"]]
      

       yline=FLY(yline)
      .data <- private$.operator$data
      .data$power<-NULL
      .data$n<-n

       out<-lapply(es,function(ind)  {
         .data$es<-ind
         powervector(private$.operator,.data)[["power"]]
         })
       z<-do.call(cbind,out)
      image$setState(list(x=x,y=y,z=z,
                          point.x=point.x,point.y=point.y,
                          n=data$n,power=data$power,yline=yline,
                          ticks=ticks,
                          tickslabels=tickslabels,
                          yticks=yticks,
                          ytickslabels=ytickslabels,
                          letter=data$letter))

    },
     .prepareNcurve = function() {
      
      if (!self$option("plot_ncurve"))
                return()
      jinfo("PLOTTER: preparing N curve plot")

        data <- private$.operator$data
        image<-private$.results$powerNcurve
      ## check the min-max for effect size
      emax <- private$.operator$data$esmax
      if (emax < data$es) emax<-data$es
      emin<-  private$.operator$data$esmin

      ## check min-max for N
      
      .data <- data
      .data$es<-ifelse(.data$es*.95 > .data$esmin, .data$es*.95, .data$esmin)
      .data$power<-.98
      .data$n <- NULL
       nmax<-powervector(private$.operator,.data)[["n"]]
      .data <- data
      .data$esmax <- emax
       nmin<-  find_min_n(private$.operator,.data)
     
      if (nmax< data$n) nmax<-data$n+10
      if (nmax<(nmin*2)) nmax=(nmin*2)

        FLX<-identity
        FEX<-identity
        FLY<-identity
        FEY<-identity

        if (self$option("plot_log")) {
            FLX<-log
            FEX<-exp
        }

       x <- seq(FLX(nmin),FLX(nmax),len=20)
       n <- FEX(x)
       point.x<-FLX(private$.operator$data$n)
       ticks<-seq(FLX(nmin),FLX(nmax),len=6)
       tickslabels<-round(FEX(ticks))
      .data<-data
      .data$n <- n
      .data$power <- NULL

       ydata <- powervector(private$.operator,.data)
       ydata$x <- x
       ydata$y <- ydata$power

       image$setState(list(data=ydata,
                            point.x = point.x,
                            point.y = private$.operator$data$power,
                            ticks=ticks,
                            tickslabels=tickslabels,
                            xlab="Required Sample Size (N)",
                            ylab="Power",
                            text=paste(data$letter,"=",round(data$es,digits=3)," ",greek_vector["alpha"],"=",round(data$sig.level,digits=3))
                          ))


    },
    .prepareEscurve = function() {
      
       if (!self$option("plot_escurve") || (self$options$aim=="n"))
               return()
        jinfo("PLOTTER: preparing Es curve plot")

        data <- private$.operator$data
        image<-private$.results$powerEscurve
    ## check the min-max for effect size
      emax <- private$.operator$data$esmax
      if (emax < data$es) emax<-data$es
      emin<-  private$.operator$data$esmin

  
        FLX<-identity
        FEX<-identity
  
        if (self$option("plot_log")) {
            if (private$.operator$loges(emax)) {
               FLX<-log
               FEX<-exp
            }
        }

       x <- seq(FLX(emin),FLX(emax),len=20)
       es <- FEX(x)
       point.x<-FLX(private$.operator$data$es)
       ticks<- FLX(pretty(c(emin,emax),6))
       tickslabels<-round(FEX(ticks),digits=3)

        .data<-data
        .data$power<-NULL
        .data$es<-es
 
        ydata<-powervector(private$.operator,.data)
        ydata$x <- x
        ydata$y <- ydata$power

     
        image$setState(list(data=ydata,
                            point.x = point.x,
                            point.y = private$.operator$data$power,
                            ticks=ticks,
                            tickslabels=tickslabels,
                            xlab="Hypothetical effect size",
                            ylab="Power",
                            text=paste("N =",data$n," ",greek_vector["alpha"],"=",round(data$sig.level,digits=3))
                          ))
    },
    
     .initCustom    = function() {
       
        if (self$option("plot_y","none")) {
          return()
        }
        if (self$option("plot_x","none")) {
          return()
        }

        if (self$options$plot_x==self$options$plot_y) {
          self$warning<-list(topic="plotnotes",message="Please define different parameters for the X and Y axis")          
          return()
        }

        if (self$options$plot_x_from==self$options$plot_x_to) {
            self$warning<-list(topic="plotnotes",message="Please set a suitable range for custom plot X-axis values")          
            return()
      }

        if (self$options$plot_x_from>self$options$plot_x_to) {
            self$warning<-list(topic="plotnotes",message="Please set a suitable range for custom plot X-axis values")          
            return()
        }
         jinfo("PLOTTER: init  custom plot")
         image<-private$.results$powerCustom
         z_values<-private$.test_z()
         y <- self$options$plot_y
         x <- self$options$plot_x
         image$setState(list(y=y, x=x, z=z_values))
       
     },
     .prepareCustom = function() {
      

        image<-private$.results$powerCustom
        if (is.null(image$state))
            return()
        z_values<-private$.test_z()

        jinfo("PLOTTER: preparing custom plot")


        data <- private$.operator$data
       
        what<-self$options$plot_x
        data[[what]]<-pretty(c(self$options$plot_x_from,self$options$plot_x_to),n=20)
        data[[what]][1]<-self$options$plot_x_from
        data[[what]][length(data[[what]])]<-self$options$plot_x_to
        
        zlab<-NULL

        if (is.something(z_values)) {
          data[[self$options$plot_z]]<-z_values
        }
        data[[self$options$plot_y]]<-NULL
        tryobj<-try_hard(powervector(private$.operator,data))
        if (!isFALSE(tryobj$error)) {
            self$warning<-list(topic="plotnotes",message="The required plot cannot be produced. Please update the plot settings")          
            return()
        } else 
           ydata<-tryobj$obj

        if (any(is.nan(ydata[[self$options$plot_y]]))) {
            self$warning<-list(topic="plotnotes",message="The required plot cannot be produced. Please update the plot settings")          
            return()
        }

        names(ydata)[names(ydata)==self$options$plot_x]<-"x"
        names(ydata)[names(ydata)==self$options$plot_y]<-"y"
        names(ydata)[names(ydata)==self$options$plot_z]<-"z"

        if (hasName(ydata,"z"))
            zlab<-nicify_param(self$options$plot_z,short=T)

        tickdata<-NULL
        if (self$option("plot_custom_labels")) {
                   ux <- unique(ydata$x)
                   ticks<-pretty(ux,n=5)
                   if (ticks[1]< min(ux)) ticks[1]<-min(ux)
                   if (ticks[length(ticks)] > max(ux)) ticks[1]<-max(ux)
                   tdata<-data
                   tdata[[self$options$plot_x]]<-ticks
                   tryobj<-try_hard(powervector(private$.operator,tdata))
                   if (isFALSE(tryobj$error)) {
                       tickdata<-tryobj$obj
                       tickdata$x<-tickdata[[self$options$plot_x]]
                       tickdata$y<-tickdata[[self$options$plot_y]]
                       if (self$options$plot_z!="none") {
                           tickdata$z<-factor(tickdata[[self$options$plot_z]])
                           
                       }

                   }
                       
        }

        image$setVisible(TRUE)
        image$setState(list(data=ydata,
                            tickdata=tickdata,
                            xlab=nicify_param(self$options$plot_x),
                            ylab=nicify_param(self$options$plot_y),
                            zlab=zlab))
    },
    .test_z = function() {
      
           z_values <- NULL
      
            if (self$option("plot_z","none"))
              return()
           
            if (self$options$plot_z %in% c(self$options$plot_x,self$options$plot_y)) {
                self$warning<-list(topic="plotnotes",message="Multiple lines cannot be plotted for the parameters in Y or X axis.")          
                return()
            }
           
            if (self$options$plot_z_lines == 0) {
                self$warning<-list(topic="plotnotes",message="Please set the required number of lines")          
                return()
            } 
          
            z_values <- unlist(lapply(self$options$plot_z_value, function(x) if (as.numeric(x) > 0) as.numeric(x)))
            
            if (is.null(z_values)) {
                self$warning<-list(topic="plotnotes",message=paste("Please set a value of",nicify_param(self$options$plot_z)," for each line."))          
                return()
             }
               
            if (length(z_values) != self$options$plot_z_lines) {
                self$warning<-list(topic="plotnotes",
                                   message=paste("Please set a value of",nicify_param(self$options$plot_z)," for each line. Only",length(z_values)," curve(s) are displayed."))          
            }
            
            return(z_values)
    
    }

  ) # end of private
) # end of class
    