.expandto<-function(alist,n) {
  
  eg<-expand.grid(alist)
  res<-list()
  l<-dim(eg)[1]
  j<-0
  for (i in 1:n) {
    j<-j+1 
    ladd(res)<-eg[j,]
    if (j==l) j=0
  }
  res<-do.call(rbind,res)
  rownames(res)<-1:dim(res)[1]
  res
}


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
    clustervalues<-as.data.frame(.nestedclusters(nclusters))
    names(clustervalues)<-clusternames
  }
  else {
    clustervalues<-expand.grid(nclusters)
  }
  
  colnames(clustervalues)<-clusternames
  clustervalues$id<-1:dim(clustervalues)[1]
  clusterdata<-list()
  dep<-rlist::list.find(vars, role=="dependent",n=1)[[1]]
  covs<-rlist::list.find(vars, type=="numeric",n=Inf)
  factors<-rlist::list.find(vars, type=="nominal",n=Inf)
  
  for (cluster in clusters) {
    ## first, we prepare a frame with the cluster values 
    cv<-data.frame(unique(clustervalues[,cluster$name]))
    names(cv)<-cluster$name
     ## we select the continuous between variables
    bvars<-rlist::list.find(covs, grep(cluster$name,role)>0,n=Inf)
    ## we produce the data (adding the dependent variable)
    bnames<-unlist(c(dep$name,rlist::list.select(bvars,name)))
    bn<-length(bnames)
    sigmab<-matrix(0,ncol=bn,nrow=bn)
    diag(sigmab)<-1
    mus<-rep(0,bn)
    ccovs<-as.data.frame(MASS::mvrnorm(cluster$n,Sigma = sigmab,mu=mus,empirical = T))
    names(ccovs)<-paste0(cluster$name,".",bnames)
    cv<-cbind(cv,ccovs)
    
    ## here we deal with between factors
    fvars<-rlist::list.find(factors, grep(cluster$name,role)>0,n=Inf)
    if (is.something(fvars)) {
       nfactors<-lapply(fvars, function(x) 1:x$n)
       fnames<-lapply(fvars, function(x) x$name)
       eg<-.expandto(nfactors,cluster$n)
       colnames(eg)<-paste0(cluster$name,".",fnames)
       cv<-cbind(cv,eg)

    }
    clusterdata[[cluster$name]]<-cv
  }

  # now the within variables  
  NC<-dim(clustervalues)[1]
  ## continuous variables
  
  cvars <- rlist::list.find(covs, role=="within",n=Inf)
  wnames<-unlist(c(dep$name,rlist::list.select(cvars,name)))
  
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
    data$id<-1:dim(data)[1]
    
  
  fvars <- rlist::list.find(factors, role=="within",n=Inf)
  

  if (is.something(fvars)) {
    nfactors<-lapply(fvars, function(x) 1:x$n)
    fnames<-lapply(fvars, function(x) x$name)
    eg<-.expandto(nfactors,dim(data)[1])
    colnames(eg)<-fnames
    data<-cbind(data,eg)
  }
  
  finaldata<-subset(data,select=clusternames)

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
    finaldata[,n]<-factor(finaldata[,n])

  finaldata$id<-1:dim(data)[1]
  as.data.frame(finaldata)
}
