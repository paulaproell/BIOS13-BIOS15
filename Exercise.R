setwd("C:/Users/pauli/Desktop/Lund/Uni/Animal Ecology/R - Processing and Analysis of Biological Data/Lecture 6 - GLM - Poisson")

#Import data
dat <- read.csv("Eulaema.csv")
library("tidyverse")

#Explore data
ggplot(dat, aes(x = Eulaema_nigrita))+
  geom_histogram(color="black",fill="lightgreen", binwidth=40)+
  labs(
    title = "Eulaema nigrita abundance",
    x="Eulaema nigrita",
    y = "count",
  )

#Explore in tidy verse
Eu.ti <- tibble(dat)
glimpse(Eu.ti)

#Exploring MAT (x10)
ggplot(dat, aes(x = MAT))+
  geom_histogram(color="black",fill="orange", binwidth=40)+
  labs(
    title = "Mean annual temperature",
    x="MAT in °C x10"
  )

#Exploring MAP
ggplot(dat, aes(x = MAP))+
  geom_histogram(color="black",fill="lightblue", binwidth=40)+
  labs(
    title = "Mean annual precipitation",
    x="MAP in mm/year"
  )

#Exploring Tseasonal
ggplot(dat, aes(x = Tseason))+
  geom_histogram(color="black",fill="orange", binwidth=40)+
  labs(
    title = "Seasonal temperature",
    x="MAT SD *100"
  )

#Exploring Pseason
ggplot(dat, aes(x = Pseason))+
  geom_histogram(color="black",fill="lightblue", binwidth=5)+
  labs(
    title = "Seasonal precipitation",
    x="CV of Pseason"
  )

#Exploring Land use
ggplot(dat, aes(x = forest.))+
  geom_histogram(color="black",fill="brown", binwidth=0.05)+
  labs(
    title = "Proportion forest cover",
    x="Percent of coverage"
  )

#Exploring Land use heterogenity
ggplot(dat, aes(x = lu_het))+
  geom_histogram(color="black",fill="brown", binwidth=0.05)+
  labs(
    title = "Land use heterogenity",
    x="Shanon Index"
  )

#Exploring effects of variables on our response variable
model1 <- glm(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + forest. +lu_het,family="poisson",data=dat)
summary(model1)
head(dat)

model2 <- glm(Eulaema_nigrita ~ MAP + Tseason + Pseason + forest. +lu_het,family="poisson",data=dat)
summary(model2)

#Explore MAP and abundance
ggplot(dat, aes(x = MAP, y = Eulaema_nigrita)) +
geom_point(color="orange") 

ggplot(dat, aes(x = lu_het, y = Eulaema_nigrita)) +
  geom_point(color="orange") 


ggplot(dat, aes(x = forest., y = Eulaema_nigrita)) +
  geom_point(color="orange") 

ggplot(dat, aes(x = lu_het, y = Eulaema_nigrita, color='forest')) +
  geom_point(color="orange") +
  scale_color_viridis_c(option = "C")

model3 <- glm(Eulaema_nigrita ~ forest. +lu_het,family="poisson",data=dat)
summary(model3)

library(MASS)
model4 <- glm.nb(Eulaema_nigrita ~ forest. +lu_het,data=dat)
summary(model4)

#plot MAP and forest. with abundance
ggplot(dat, aes(x = forest., y = Eulaema_nigrita, color = `MAP`)) +
  geom_point(size = 3) +
  scale_color_gradient(low="lightblue", high="darkblue") +  
  labs(
    x = "Forest cover (proportion)",
    y = "Eulaema nigrita abundance",
    color = "Mean annual precipitation (mm)"
  ) 

plot(model4)

#Fit a model using forest cover and MAP
model5 <- glm.nb(Eulaema_nigrita ~ forest. + MAP, data=dat)
summary(model5)

#Plot model
#give specific x vlaues and put it in our funtion and in plot
x_values <- seq(min(dat$MAP), max(dat$MAP))
l <- exp(6.6792933) + exp(-1.3118618) * dat$MAP
plot(l)

exp(6.6792933)      #if variables=0 bee count =797.56
exp(-0.0013900)     #nr of bees when everything is 0  + mean of rain
exp(-1.3118618)     #nr of bees when everything is 0  + mean of forest cover

modelplot <- exp(6.6792933) * exp(-1.3118618) * dat$Eulaema_nigrita
plot(modelplot)
