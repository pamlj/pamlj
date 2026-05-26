testthat::context("mixed")
tol<-.0001

m<-"
y~0*1+.65*x+a*.3*z+(1*1+5.8*x|cluster)
bet:a|cluster
test:a
"
pamlj::pamlmixed(aim="n",find="n",syntax = m,clusterpars = list(cluster=c(n=10,k=20)),algo="raw")


