###Bee distribution

#As an example, we will ask whether the local abundance of *Eulaema nigrita* depends on forest cover, while accounting for variation in yearly rainfall. We could write the analysis methods as follows:

#To assess the response of *Eulaema nigrita* to variation in forest cover, we fitted a generalized linear model with negative binomial error distribution (accounting for severe overdispersion) and a log link function. To account for the expected greater abundance of the study species in drier areas, we included mean-centered mean annual precipitation as a covariate in the model.


library(MASS)

dat <- read.csv("YOURWORKINGDIRECTORY/Eulaema.csv") 
str(dat)
dat$mcMAP = dat$MAP - mean(dat$MAP, na.rm=T)#mean center to get estimates at mean MAP

hist(dat$Eulaema_nigrita)

#start with glm with poisson and nbGLM. Compare QQ plot to see which model works better

m_glm = glm(Eulaema_nigrita ~ mcMAP + forest.,  "poisson", data = dat)

par(mfrow=c(2,2))
plot(m_glm)
summary(m_glm)

exp(-1.637)

m = glm.nb(Eulaema_nigrita ~ mcMAP + forest., data = dat)
summary(m)

par(mfrow=c(2,2))
plot(m)

#nb GLM seems to work way better. So, we first interpret this model.

#To interpret the parameter estimates, it is easiest to backtransform the predicted values for some relevant scenarios to the original count scale. Because MAP is mean-centered, the intercept corresponds to the the number of bees counted when forest cover i zero and MAP is set to its mean. Because forest cover ranges from 0 to 1, an one unit increase corresponds to the entire data range, and we can easily compute the predicted number of bees in a fully forested landscape by simply adding the forest-cover parameter estimate to the intercept.

coefs = summary(m)$coef
exp(coefs[1,1])
exp(coefs[1,1] + coefs[3,1])

plot(dat$forest., dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab=expression(paste(italic("El. nigrita")," abundance")))

newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200)
newMAP = rep(mean(dat$mcMAP), length(newforest))
y_hat = predict(m, newdata=list(mcMAP=newMAP, 
                                forest.=newforest), 
                type="response")
lines(newforest, y_hat, lwd=2)

newMAP2 = rep(mean(dat$mcMAP)+sd(dat$mcMAP), length(newforest))
y_hat2 = predict(m, newdata=list(mcMAP=newMAP2, 
                                forest.=newforest), 
                type="response")

newMAP3 = rep(mean(dat$mcMAP)-sd(dat$mcMAP), length(newforest))
y_hat3 = predict(m, newdata=list(mcMAP=newMAP3, 
                                forest.=newforest), 
                type="response")

lines(newforest, y_hat2, lwd=2, col="blue3")
lines(newforest, y_hat3, lwd=2, col="firebrick")

legend("topleft", lty=1, lwd=2, col=c(1, "blue3", "firebrick"), bty="n", 
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))

abline(h=exp(coefs[1,1]), lty=2)
abline(h=exp(coefs[1,1] + coefs[3,1]), lty=2)

#What if we use estimates from Poisson model?

plot(dat$forest., dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab=expression(paste(italic("El. nigrita")," abundance")))

newMAP = rep(mean(dat$mcMAP), length(newforest))
y_hat = predict(m_glm, newdata=list(mcMAP=newMAP, 
                                forest.=newforest), 
                type="response")
lines(newforest, y_hat, lwd=2)

newMAP2 = rep(mean(dat$mcMAP)+sd(dat$mcMAP), length(newforest))
y_hat2 = predict(m_glm, newdata=list(mcMAP=newMAP2, 
                                forest.=newforest), 
                type="response")

newMAP3 = rep(mean(dat$mcMAP)-sd(dat$mcMAP), length(newforest))
y_hat3 = predict(m_glm, newdata=list(mcMAP=newMAP3, 
                                forest.=newforest), 
                type="response")

lines(newforest, y_hat2, lwd=2, col="blue3")
lines(newforest, y_hat3, lwd=2, col="firebrick")

legend("topleft", lty=1, lwd=2, col=c(1, "blue3", "firebrick"), bty="n", 
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))