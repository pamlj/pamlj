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
      },

      initPlots=function() {
        

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
      
        filled.contour(data$x,data$y,data$z,color.palette =  paml_palette,
               key.title = {mtext("Power",3, .5)},
               ylab = expression(paste("Hypothetical effect size (",rho,")", sep = "")),
               xlab="Sample Size (N)",
               plot.axes={
                  axis(1)
                  axis(2)
                  lines(data$x, data$yline, type = "l", lty = 1, lwd=2)
                  segments(0,data$point.y,data$point.x,data$point.y, lwd=2)
                  segments(data$point.x,0,data$point.x,data$point.y, lwd=2)
                  points(data$point.x,data$point.y,pch=21,bg="white",cex=1.5)

               })

      },
      plot_curve= function(image,ggtheme,theme) {
         
        if (!private$.operator$ok) return()
        if (!self$option("plot_ncurve") && !self$option("plot_escurve"))
                return()

         cols = paml_palette(10)
         data<-image$state
         range <- max(data$x)-min(data$x)
         plot(data$x,data$y,  ty='n',
              ylab=data$ylab, xlab=data$xlab,
              xlim=c(min(data$x)+.1*range,max(data$x)-.1*range),
              ylim=c(0,1)
              )
         yrect <- seq(0,1,.1)
         yrect[1]<-yrect[1]-.5
         yrect[11]<-yrect[11]+.5

         for(i in 1:10){
              rect(par()$usr[1], yrect[i], par()$usr[2], yrect[i+1], border = NA,
                   col = cols[i])
         }
         lines(data$x,data$yline,lwd=2)
         segments(0,data$point.y,data$point.x,data$point.y, lwd=2)
         segments(data$point.x,0,data$point.x,data$point.y, lwd=2)
         points(data$point.x,data$point.y,pch=21,bg="white",cex=1.5)
         mtext(data$text, adj = 1)
       },
      plot_custom= function(image,ggtheme,theme) {
         
        if (!private$.operator$ok) return()
        if (!self$option("plot_custom"))
                return()
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
      jinfo("PLOTTER: preparing contour plot")
      
      data <- private$.operator$data
      image<-private$.results$powerContour
      ## notice that we send the 'aes' (actual effect size), already transformed
      data$es<-data$es*.95
      data$power<-.98
      data$n <- NULL
      nmax<-powervector(private$.operator,data)
      if (nmax<10) nmax=10
      nmin<-max(5,data$df_model+3)
      x=round(seq(nmin,round(nmax),len=20))
      data <- private$.operator$data
      data$n<-x
      data$es <- NULL
      yline=powervector(private$.operator,data)
      data <- private$.operator$data
      data$power<-NULL
      y<-seq(.01,1,.1)
      xyz.func<- function(x,y) {
             data$n<-x
             data$es<-y
             powervector(private$.operator,data)
      }

      z<-outer(x,y,xyz.func)
      point.x <- private$.operator$data$n
      point.y<-  private$.operator$data$es
      image$setState(list(x=x,y=y,z=z,
                          point.x=point.x,point.y=point.y,
                          n=data$n,power=data$power,yline=yline))
    
    },
     .prepareNcurve = function() {
      
        if (!self$option("plot_ncurve"))
                return()
        jinfo("PLOTTER: preparing N curve plot")

        data <- private$.operator$data
        image<-private$.results$powerNcurve
        ## notice that we send the 'es' (actual effect size), not transformed
        mes<-data$es*.95
        nmax<-powervector(private$.operator,list(power=.98,es=mes,alpha=data$alpha))
        if (nmax<10) nmax=10
        nmin<-max(5,data$df_model+5)
        x=round(seq(nmin,round(nmax),len=20))
        yline<-powervector(private$.operator,list(n=x,es=data$es,alpha=data$alpha))
        point.x <- private$.operator$data$n
        point.y<-  private$.operator$data$power
        image$setState(list(x=x,y=yline,
                          point.x=point.x,point.y=point.y,
                          n=data$n,power=data$power,yline=yline,
                          ylin=private$.operator$info$es_lim,
                          xlab="Sample Size (N)",
                          ylab="Power",
                          text=paste(data$letter,"=",data$es," ",greek_vector["alpha"],"=",round(data$alpha,digits=3))
                       ))

    },
    .prepareEscurve = function() {
      
        if (!self$option("plot_escurve"))
                return()
        jinfo("PLOTTER: preparing Es curve plot")

        data <- private$.operator$data
        image<-private$.results$powerEscurve
        x<-seq(0.01,1,len=20)
        yline<-powervector(private$.operator,list(n=data$n,es=x,alpha=data$alpha))
        point.x <- private$.operator$data$es
        point.y<-  private$.operator$data$power
        image$setState(list(x=x,y=yline,point.x=point.x,point.y=point.y,n=data$n,power=data$power,yline=yline,
                          xlab="Hypothetical effect size",
                          ylab="Power",
                          text=paste("N =",data$n," ",greek_vector["alpha"],"=",round(data$alpha,digits=3))
                          ))
    },
    
     .prepareCustom = function() {
      
        private$.results$plotnotes$setContent(" ")
        if (!self$option("plot_custom"))
                return()

        if (self$option("plot_y","none")) {
          self$warning<-list(topic="plotnotes",message="Please specify the custom plot Y-axis values")          
          return()
        }
        if (self$option("plot_x","none")) {
          self$warning<-list(topic="plotnotes",message="Please specify the custom plot X-axis values")          
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

        goz<-FALSE
        if (!self$option("plot_z","none")) {
            goz<-TRUE
            if (self$options$plot_z %in% c(self$options$plot_x,self$options$plot_y)) {
                self$warning<-list(topic="plotnotes",message="Multiple lines cannot be plotted for the parameters in Y or X axis.")          
                goz<-FALSE
            }
            
            if (goz && self$options$plot_z_from==self$options$plot_z_to) {
                self$warning<-list(topic="plotnotes",message="Please set a suitable range for multiple lines. This command is ignored")          
                goz<-FALSE
            }
            if (goz && self$options$plot_z_from>self$options$plot_z_to) {
                self$warning<-list(topic="plotnotes",message="Please set a suitable range for multiple lines. This command is ignored")          
                goz<-FALSE
            }
            if (goz && self$options$plot_z_by==0) {
                self$warning<-list(topic="plotnotes",message="Number of lines has been set to 2")          
                goz<-FALSE
            }
        }

        jinfo("PLOTTER: preparing custom plot")


        data <- private$.operator$data
        image<-private$.results$powerCustom
        what<-self$options$plot_x
        data[[what]]<-pretty(c(self$options$plot_x_from,self$options$plot_x_to),n=20)
        zlab<-NULL
        if (goz) {
          data[[self$options$plot_z]]<-c(self$options$plot_z_from,self$options$plot_z_to)
          zlab<-nicify_param(self$options$plot_z)
        }
        data[[self$options$plot_y]]<-NULL
        tryobj<-try_hard(powervector(private$.operator,data))
        if (!isFALSE(tryobj$error)) {
            self$warning<-list(topic="plotnotes",message="The required plot cannot be produced. Please update the plot settings")          
            return()
        } else 
           ydata<-tryobj$obj

        ydata<-powervector(private$.operator,data)
        if (any(is.nan(ydata[[self$options$plot_y]]))) {
            self$warning<-list(topic="plotnotes",message="The required plot cannot be produced. Please update the plot settings")          
            return()
        }
        names(ydata)[names(ydata)==self$options$plot_x]<-"x"
        names(ydata)[names(ydata)==self$options$plot_y]<-"y"
        names(ydata)[names(ydata)==self$options$plot_z]<-"z"

        tickdata<-NULL
        if (self$option("plot_custom_labels")) {
                   ticks<-pretty(unique(ydata$x),n=5)
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

        image$setState(list(data=ydata,
                            tickdata=tickdata,
                            xlab=nicify_param(self$options$plot_x),
                            ylab=nicify_param(self$options$plot_y),
                            zlab=zlab))
    }
    

  ) # end of private
) # end of class
    