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
      mes<-data$es*.95
      nmax<-powervector(private$.operator,list(power=.98,es=mes,alpha=data$alpha))
      if (nmax<10) nmax=10
      nmin<-max(5,data$df_model+5)
      x=round(seq(nmin,round(nmax),len=20))
      
      yline=powervector(private$.operator,list(n=x,power=data$power,alpha=data$alpha))
  
      y<-seq(.01,1,.1)

      xyz.func<- function(x,y) {
             powervector(private$.operator,list(n=x,es=y,alpha=data$alpha))
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
        ## notice that we send the 'aes' (actual effect size), already transformed
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
                          text=paste("N =",data$es," ",greek_vector["alpha"],"=",round(data$alpha,digits=3))
                          ))
    }
    

    

  ) # end of private
) # end of class
    