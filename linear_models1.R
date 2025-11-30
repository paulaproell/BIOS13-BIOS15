setwd("C:\\Users\\pauli\\Desktop\\Lund\\Uni\\Animal Ecology\\R - Processing and Analysis of Biological Data\\Lecture 2")

m = lm(y~x) #make a linear function


set.seed(85)                   #set seed so every time same result
x = rnorm(n=200, mean=10, sd=2)           #random distribution with values
y = 0.4*x + rnorm(200, 0, 1) 
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")


cf = m$coef                           #calculate coefficient and index it
cf


predvals = cf[1] + cf[2]*x                   #calculate intercept and slope
par(mfrow=c(1,2))                            #make two graphs one next to other
plot(x, y, las=1,                            #y-axis horizontal
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
abline(m)                          #make a regression line
segments(x, y, x, predvals)        #vertical line to calculated points
hist(residuals(m), xlab="", las=1)       #show residuals

newx = seq(min(x), max(x), length.out=200)    
predy = cf[1] + cf[2]*newx
plot(x, y, las=1,                     #plots points
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)                  #makes line                   
abline(m)                           #extends line
segments(x, y, x, predvals)         #line from line to point (residuals)
hist(residuals(m), xlab="", las=1)   #histogram of residuals 

summary(m)

df = data.frame(x, y)
head(df)

cov(y,x)/var(x)

(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x))    #change in y for mean + 1sd
