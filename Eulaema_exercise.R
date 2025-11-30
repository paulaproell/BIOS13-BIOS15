#Eulaema nigrita exercise

#Open packages
library("tidyverse")
library(MASS)

#Import data
dat <- read.csv("Eulaema.csv")

#Explore data
names(dat)
head(dat)

#Explore in tidy verse
dat.tib <- tibble(dat)
glimpse(dat.tib)

#Visualize data
ggplot(dat, aes(x = Eulaema_nigrita))+
  geom_histogram(color="black",fill="greenyellow", binwidth=40)+
  labs(
    x="Eulaema nigrita abundance",
    y = "count",
  ) +
  theme_classic()


#Exploring forest cover
ggplot(dat, aes(x = forest.))+
  geom_histogram(color="black",fill="darkgreen", binwidth=0.05)+
  labs(
    x="Percentage of forest cover", 
    y = "count"
  ) +
  ylim(0,25) +
  theme_classic()


#Exploring MAP 
ggplot(dat, aes(x=MAP))+
  geom_histogram(color="black",fill="lightblue", binwidth=100)+
  labs(
    x="Mean anual precipitation (mm)",
    y = "count"
  ) +
  theme_classic()

#Fitting a GLM model with poisson distribution to our data
model1 <- glm(Eulaema_nigrita ~ forest. + MAP,family="poisson",data=dat)
summary(model1)

#Fitting a GLM model with negative bionomial distribution to our data
model2 <- glm.nb(Eulaema_nigrita ~ forest. + MAP, data=dat)
summary(model2)

#Calculating percentage explained by our model
#null deviance - residual deviance) / null deviance
percentexp <- (1 - (model2$deviance/model2$null.deviance)) * 100

#calculating exp(β) values
exp(6.6792933)
exp(-1.3118618)
exp(-0.0013900)

#Exploring other relevant values of Eualaema nigrita abundance
mean(dat$Eulaema_nigrita)
range(dat$Eulaema_nigrita)

#Plotting variables together
ggplot(dat, aes(x = forest., y = Eulaema_nigrita, color = MAP)) +
  geom_point(size = 4) +
  scale_color_gradient(low= "lightblue", high="darkblue") +  
  labs(
    x = "Percentage of forest cover",
    y = "Eulaema nigrita abundance",
    color = "MAP (mm)"
  ) +
  theme_classic()

#Exploring data at intercept and + mean forest cover and + mean MAP
coefs = summary(model2)$coef
exp(coefs[1,1])                         #Eulaema_nigrita abundance at forest cover=0 and heterogenity index=0
exp(coefs[1,1] + coefs[2,1] * mean(dat$forest.))  #Eulaema_nigrita abundance at mean forest cover
exp(coefs[1,1] + coefs[3,1] * mean(dat$lu_het))   #Eulaema_nigrita abundance at mean anual precipitation

#Plot Eulaema nigrita abundance against forest cover
plot(dat$forest., dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab="Eulaema nigrita abundance")

#Calculate predicted values
newforest <- seq(min(dat$forest.), max(dat$forest.), length.out=200)
newMAP <- rep(mean(dat$MAP), length(newforest))     
y_hat = predict(model2, newdata=list(
  MAP = newMAP,
  forest. = newforest), 
  type="response")

#Calculate + and - SD
MAP_plussd = rep(mean(dat$MAP)+sd(dat$MAP), length(newforest))
y_hatplussd = predict(model2, newdata=list(
  MAP = MAP_plussd, 
  forest.=newforest), 
  type="response")

MAP_minussd = rep(mean(dat$MAP)-sd(dat$MAP), length(newforest))
y_hatminussd = predict(model2, newdata=list(
  MAP = MAP_minussd , 
  forest. = newforest), 
  type="response")


#Plot +SD and -SD of predicted response
lines(newforest, y_hat, lwd=2)
lines(newforest, y_hatplussd, lwd=2, col="darkblue")
lines(newforest, y_hatminussd , lwd=2, col="lightblue")

legend("topleft", lty=1, lwd=2, col=c(1, "darkblue", "lightblue"), bty="n", 
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))
