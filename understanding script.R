#CV =(√σ2)/µ
#CV=(sqrt(σ)^2)/µ

x <- rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)

#set.seed(1)
#x = rnorm(50, 10, 2)
#se_x = sqrt(var(x)/length(x)) #se

out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  5
  out[i] = mean(sample)
}
out
sd(out)
mean(out)
quantile(out, c(0.025, 0.975))
