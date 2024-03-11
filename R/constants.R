j_DEBUG=T
j_INFO=T
t_INFO=T



TRANS_WARNS<-list()

INTRO<-"<b> Getting started</b>:<br> To produce a dataset useful for simulations, please start with an empty dataset.
        Define the name of the <b>dependent variable</b> and the number of participants per cluster.
        Then specify the number of clusters for each <b>clustering variable</b>.
        The total sample N will be `N per cluster` times `N clusters`. 
        In case of multiple clustering variables, `N per cluster` is the N within each combinations of clusters
        <br>For <b>factors</b>, please specify the number of levels (categories) to simulate.<br>
        If the dataset is not empty, the name of the variables to simulate should not be present in the dataset. <br>
        When the setup is ok, please select <b>Produce the data</b> option to transfer the
        data in the dataset. <br>
        All variables are uncorrelated, their relationships can be defined in the power analysis module."

BRIEF<-"<b>Available actions</b>:<br> One can use the produced data to estimate power parameters
         of the mixed model. Although one can work directly on the active dataset, it is reccomanded
         to save the data as a <b>csv file</b>, so the data will not be altered in future sessions."




########### Greek Letters  ###############

greek_vector <- c( # lowercase Greek letters
  alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
  eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
  nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
  upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
  # uppercase Greek letters
  Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
  Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
  Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
  Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
  # mathematical symbols
  infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
  emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
  asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
  leq='\u2264', lt="\u003c",gt="\u003e", geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
  equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
  logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')


letter_chi2<-paste(greek_vector["chi"],'\u00B2',sep="")
letter_eta2<-paste(greek_vector["eta"],'\u00B2',sep="")
letter_peta2<-paste(greek_vector["eta"],'\u00B2',"p",sep="")
letter_omega2<-paste(greek_vector["omega"],'\u00B2',sep="")
letter_pomega2<-paste(greek_vector["omega"],'\u00B2',"p",sep="")
letter_epsilon2<-paste(greek_vector["epsilon"],'\u00B2',sep="")
letter_pepsilon2<-paste(greek_vector["epsilon"],'\u00B2',"p",sep="")


##########################

