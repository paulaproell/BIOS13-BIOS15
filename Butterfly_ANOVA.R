#ANOVA butterfly exercise

#Load packages and data
library(tidyverse)
dat <- read.csv("butterflies.csv")                         
names(dat)                                     

#ANOVA - Development Time        
dt <- lm(DevelopmentTime ~ MaternalHost* LarvalHost, data=dat)
anova(dt)        
summary(dt)

#Development time - Summarize mean values and SE 
dat %>%
  group_by(MaternalHost, LarvalHost) %>%
  summarise(
    mean_dt <- mean(DevelopmentTime),
    se_dt <- sd(DevelopmentTime)/sqrt(n())
  )

#ANOVA - Adult Weight
aw <- lm(AdultWeight ~ MaternalHost * LarvalHost, data=dat)
anova(aw)        
summary(aw)

#Adult weight - Summarize mean values and SE 
dat %>%
  group_by(MaternalHost, LarvalHost) %>%
  summarise(
    mean_aw <- mean(AdultWeight),
    se_aw <- sd(AdultWeight)/sqrt(n())
  )

#ANOVA - Growth Rate
gr <- lm(GrowthRate ~ MaternalHost * LarvalHost, data=dat)
anova(gr)        
summary(gr)

#Growth rate Summarize mean values and SE 
dat %>%
  group_by(MaternalHost, LarvalHost) %>%
  summarise(
    mean_gr <- mean(GrowthRate),
    se_gr <- sd(GrowthRate)/sqrt(n())
  )


#Boxplots in ggplot
#specify data, x, y, fill or color, add layers, add labels

#Development Time
ggplot(dat, aes(LarvalHost, DevelopmentTime, fill=MaternalHost)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("Barbarea"="pink", "Berteroa"="yellow")) +
  labs(
    x = "Larval Host Plant",      
    y = "Development Time (days)",
    fill = "Maternal Host Plant"
  )

#Adult Weight
ggplot(dat,aes(LarvalHost, AdultWeight, fill=MaternalHost)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("Barbarea"="pink", "Berteroa"="yellow")) +
  labs(
    x = "Larval Host Plant",      
    y = "Adult Weight (mg)",
    fill = "Maternal Host Plant"
  )

#Growth Rate
ggplot(dat,aes(LarvalHost, GrowthRate, fill=MaternalHost)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("Barbarea"="pink", "Berteroa"="yellow")) +
  labs(
    x = "Larval Host Plant",      
    y = "Growth rate (1/day)",
    fill = "Maternal Host Plant"
  )
