testthat::context("mixed")
tol<-.0001

m<-"
y~0*1+.65*x+.9*z+(1*1+.8*x|cluster)
b~~.2*c
a==0
"
pamlj::pamlmixed(aim="n",find="n",syntax = m,clusterpars = list(list(name="cluster",n=10,k=20)),mcR = 500)


