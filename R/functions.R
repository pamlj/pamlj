.levelstandard<-function(data,y,var,cluster,id="id",level="within") {
  
  
  if (!(id %in% names(data)))
    stop("Variable id,",id,"not present in the dataframe")
  zname<-paste0("z",var)
  
  if (level=="between") {
    xdata<-data
    s0<-tapply(xdata[[y]], list(xdata[[cluster]]), mean)
    mm<-sd(s0)
    print(mm)
    xdata[[zname]]<-as.numeric(scale(data[[var]],scale=1/mm))
    
  } else {
    s0<-tapply(data[[y]], list(data[[cluster]]), sd)
    mm<-mean(s0)
    vdata<-c()
    cls<-unique(data[[cluster]])
    tot<-length(cls)  
    i<-0
    for (cl in cls) {
      i<-i+1
      cat("\r standardizing  cluster", i, "of", tot) 
      flush.console()
      ldata<-data[data[[cluster]]==cl,c(id,var)]
      ldata[[zname]]<-as.numeric(scale(ldata[[var]],scale=1/mm))
      ldata[[2]]<-NULL
      vdata<-rbind(ldata,vdata)
      xdata<-merge(data,vdata,by="id")
    }   
  }
  xdata  
  
}

.nestedclusters=function(clusters) {
  
  eg<-rev(expand.grid(rev(clusters)))
  alist<-lapply(1:(ncol(eg)), function(i) {
    a<-eg[,i]*10^(ncol(eg)-i)
    a
  }  )
  eg<-do.call(cbind,alist)
  res<-eg
  for (i in 1:nrow(eg))
    for (j in 1:ncol(eg))
      res[i,j]<-sum(eg[i,1:j])/(10^(ncol(eg)-j))
  res
}     


.asample<-function(vars,clusters,type) {
  
  nclusters<-lapply(clusters, function(x) 1:x$n)
  clusternames<-unlist(rlist::list.select(clusters,name))

  if (type=="nested") {
    clustervalues<-.nestedclusters(nclusters)
  }
  else {
    clustervalues<-expand.grid(nclusters)
  }
  colnames(clustervalues)<-clusternames
  clustervalues$id<-1:dim(clustervalues)[1]
  clusterdata<-list()
  dep<-rlist::list.find(vars, role=="dependent",n=1)[[1]]
  
  for (cluster in clusters) {
    
    bvars<-rlist::list.find(vars, grep(cluster$name,role)>0,n=Inf)
    bnames<-unlist(c(dep$name,rlist::list.select(bvars,name)))
    bn<-length(bnames)
    sigmab<-matrix(0,ncol=bn,nrow=bn)
    diag(sigmab)<-1
    mus<-rep(0,bn)
    clvalues<-unique(clustervalues[,cluster$name])
    n<-length(clvalues)
    cv<-as.data.frame(MASS::mvrnorm(n,Sigma = sigmab,mu=mus,empirical = T))
    names(cv)<-paste0(cluster$name,".",bnames)
    cv[[cluster$name]]<-clvalues
    clusterdata[[cluster$name]]<-cv
  }
  
  
  NC<-dim(clustervalues)[1]
  wvars<-rlist::list.find(vars, role=="within",n=Inf)
  wnames<-unlist(c(dep$name,rlist::list.select(wvars,name)))
  wn<-length(wnames)
  sigmaw<-matrix(0,ncol=wn,nrow=wn)
  diag(sigmaw)<-1

  mus<-rep(0,wn)
  ldata<-list()
  for (i in 1:NC) {
    cv<-as.data.frame(MASS::mvrnorm(dep$n,Sigma = sigmaw ,mu=mus,empirical = T))
    names(cv)<-wnames
    cls<-subset(clustervalues,id==i)
    cls[["id"]]<-NULL

    for (cl in names(cls)) {
      cv[,cl]<-cls[[cl]]
      cv<-merge(cv,clusterdata[[cl]],by=cl,all.x = T)
    }
    ldata[[length(ldata)+1]]<-cv
    
  }
  
  data<-as.data.frame(do.call(rbind,ldata))
  finaldata<-subset(data, select = clusternames)
  
  var<-vars[[2]]
  for (var in vars) {
    if (paste0(var$role,collapse = "")!="within") {
      re<-paste0("\\.",var$name,"$")
      adata<-data[,grep(re,names(data))]
      if (class(adata)=="numeric") {
        adata<-data.frame(x=adata)
        names(adata)<-var$name
      }
      if (var$name %in% names(data))
        adata[,".q."]<-data[,var$name]
      
      if (!is.null(ncol(adata)))
        finaldata[[var$name]]<-apply(adata,1,sum)
      else
        finaldata[[var$name]]<-adata
    } else 
      finaldata[[var$name]]<-data[,var$name]
    
  }
  for (n in clusternames) 
    finaldata[,n]<-factor(data[,n])
  
  finaldata$id<-1:dim(data)[1]
  as.data.frame(finaldata)
}
