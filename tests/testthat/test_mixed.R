testthat::context("mixed")
tol<-.0001

m<-"
y~0*1+.65*x+a*.3*z+(1*1+5.8*x|cluster)+(1*1|cluster2)
bet:x|cluster
within:z|cluster
test: a
expand: cluster2
"
pamlj::pamlmixed(aim="n",find="k",syntax = m,clusterpars = list(cluster=c(n=10,k=20),cluster2=c(n=2,k=10)),algo="raw",sigma2 = 30)


