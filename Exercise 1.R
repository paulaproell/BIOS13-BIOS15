#Exercise 1
# bootstrapping to derive a 95% confidence interva for CV of a variable

cv<-function(x){
  ccv<-sd(x)/mean(x)
  return(ccv)
}

set.seed(1)
m<-rnorm(100,10,2)


out.cv<-NULL
for(i in 1:1000){
  sample.cv<-sample(m,replace=TRUE)
out.cv[i]<-cv(sample.cv)
}

cat(cv(out.cv))
quantile(out.cv,c(0.025, 0.975))