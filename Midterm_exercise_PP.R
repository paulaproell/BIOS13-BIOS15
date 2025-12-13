setwd("C:/Users/pauli/Desktop/Lund/Uni/Animal Ecology/R - Processing and Analysis of Biological Data/Midterm exercise")

#Midterm exercise
#Open packages
library("tidyverse")
library("MASS")

#Load data
dat <- read.csv("exam2023_data-2.csv")
metdat <- read.csv("exam2023_metadata-2.csv")

#Explore data and metadata
names(dat)
head(dat)

names(metdat)
head(metdat)

#Explore in tidy verse
dat.tib <- tibble(dat)
glimpse(dat.tib)

#Visualize data
#Pool seedlings
dat_long <- dat %>%
  pivot_longer(
    cols = c(euc_sdlgs0_50cm, euc_sdlgs50cm.2m, euc_sdlgs.2m),
    names_to = "size_class",
    values_to = "count"
  )%>%
  mutate(
    size_class = dplyr::recode(
      size_class,
      euc_sdlgs0_50cm = "0–50cm",
      euc_sdlgs50cm.2m = "50cm–2m",
      euc_sdlgs.2m = "2m"
    )
  ) 


#Plot data
dat_long$size_class <- factor(dat_long$size_class,
                              levels = c("0–50cm", "50cm–2m", "2m")) #order variables

ggplot(dat_long, aes(x = count, fill = size_class)) +
  geom_histogram(color = "black", binwidth = 5) +
  scale_fill_manual(
    values = c("0–50cm" = "lightgreen",
               "50cm–2m" = "green",
               "2m" = "darkgreen"
               )
  ) +
  labs(
    x = "Nr of Eucalyptus seedlings",
    y = "Count",
    fill = "Size class"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top")
  )


#Pool total seedlings together
dat2 <- dat %>%
  mutate(total_seedlings =
           euc_sdlgs0_50cm +
           euc_sdlgs50cm.2m +
           euc_sdlgs.2m)

names(dat2)

#Plot number of seedling in the different sampling points
ggplot(dat2, aes(x = SurveyID, y = total_seedlings)) +
  geom_point(size=2) +
  labs(x = "Position", y = "Total seedlings") +
  theme_classic()

#Identify outlier
which(dat2$total_seedlings == max(dat2$total_seedlings))

#Remove outlier
dat3 <- dat2 %>% 
  slice(-44)

#Plot data again without outlier
ggplot(dat3, aes(x = SurveyID, y = total_seedlings)) +
  geom_point(size=2) +
  labs(x = "Position", y = "Total seedlings") +
  theme_classic()

#Explore variables
model1 <- glm(total_seedlings ~ Distance_to_Eucalypt_canopy.m. + PET + MrVBF,family="poisson",data=dat3)
summary(model1)

model2 <- glm.nb(total_seedlings ~  Distance_to_Eucalypt_canopy.m. + PET + 
                   MrVBF,data=dat3)
summary(model2)

#Summary statistics
#Calculating exp(β) values
coefs = summary(model2)$coef
exp(coefs[1,1])
exp(coefs[2,1])
exp(coefs[3,1])
exp(coefs[4,1])

#Calculate pseudo R^2
1 - model2$deviance/model2$null.deviance

#Mean and range of response variable
mean(dat3$total_seedlings)
range(dat3$total_seedlings)

#Explore effect of variables
exp(coefs[1,1]) #Seedling abundance when all other predictors =0                   
exp(coefs[1,1] + coefs[2,1] * mean(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE)) #at mean DEC
exp(coefs[1,1] + coefs[3,1] * mean(dat3$PET, na.rm = TRUE))   #at mean PET
exp(coefs[1,1] + coefs[4,1] * mean(dat3$MrVBF, na.rm = TRUE))   #at mean MrVBF


#Make graphs with different variables
#Distance to Eucalyptus canopy
ggplot(dat3, aes(x = Distance_to_Eucalypt_canopy.m., y = total_seedlings)) +
  geom_point(size=3, color="darkgreen") +
  labs(x = "Distance to Eucalyptus canopy", y = "Total seedlings") +
  theme_classic()

#Potential evapotranspiration
ggplot(dat3, aes(x = PET, y = total_seedlings)) +
  geom_point(size=3, color="darkorange") +
  labs(x = "Potential evapotranspiration", y = "Total seedlings") +
  theme_classic()

#Flatness index
ggplot(dat3, aes(x = MrVBF, y = total_seedlings)) +
  geom_point(size=3, color="brown")+
  labs(x = "Flatness index", y = "Total seedlings") +
  theme_classic()


#Make predictions with Distance to eucalyptus canopy
range(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE)  #explore range ignoring NA's

newdec <- seq(min(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE), 
              max(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE), 
              length.out = 65)

neweuc <- rep(mean(dat3$total_seedlings), length(newdec))     

newdata <- data.frame(
  Distance_to_Eucalypt_canopy.m. = newdec,
  PET = mean(dat3$PET, na.rm = TRUE),
  MrVBF = mean(dat3$MrVBF, na.rm = TRUE)
)

y_hat <- predict(model2, newdata = newdata, type = "response")

# Get predictions on link scale with standard errors
pred_link <- predict(model2, newdata = newdata, type = "link", se.fit = TRUE)

# Compute 95% CI on response scale
newdata$predicted <- exp(pred_link$fit)          # predicted counts
newdata$lower <- exp(pred_link$fit - 1.96*pred_link$se.fit)
newdata$upper <- exp(pred_link$fit + 1.96*pred_link$se.fit)

# Plot
ggplot() +
  geom_jitter(data = dat3, aes(x = Distance_to_Eucalypt_canopy.m., y = total_seedlings),
              width = 0.1, alpha = 0.6, color = "grey30") +
  geom_line(data = newdata, aes(x = Distance_to_Eucalypt_canopy.m., y = predicted), color = "black", size = 1.2) +
  geom_ribbon(data = newdata, aes(x = Distance_to_Eucalypt_canopy.m., ymin = lower, ymax = upper),
              fill = "darkgreen", alpha = 0.4) +
  labs(x = "Distance to Eucalyptus canopy (m)", y = "Total seedlings") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )

# Explore range of PET
range(dat3$PET, na.rm = TRUE)  # explore range ignoring NA's

# Create sequence of PET values
newPET <- seq(min(dat3$PET, na.rm = TRUE), 
              max(dat3$PET, na.rm = TRUE),
              length.out = 1400)
              

# Create newdata with PET varying, other predictors at mean
newdata <- data.frame(
  Distance_to_Eucalypt_canopy.m. = mean(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE),
  PET = newPET,
  MrVBF = mean(dat3$MrVBF, na.rm = TRUE)
)

# Predict on response scale
y_hat <- predict(model2, newdata = newdata, type = "response")

# Get predictions on link scale with standard errors
pred_link <- predict(model2, newdata = newdata, type = "link", se.fit = TRUE)

# Compute 95% CI on response scale
newdata$predicted <- exp(pred_link$fit)          # predicted counts
newdata$lower <- exp(pred_link$fit - 1.96*pred_link$se.fit)
newdata$upper <- exp(pred_link$fit + 1.96*pred_link$se.fit)

# Plot
ggplot() +
  geom_jitter(data = dat3, aes(x = PET, y = total_seedlings),
              width = 0.1, alpha = 0.6, color = "grey30") +
  geom_line(data = newdata, aes(x = PET, y = predicted), color = "black", size = 1.2) +
  geom_ribbon(data = newdata, aes(x = PET, ymin = lower, ymax = upper),
              fill = "orange", alpha = 0.4) +
  labs(x = "Potential Evapotranspiration (PET)", y = "Total seedlings") +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )

# Explore range of MrVBF
range(dat3$MrVBF, na.rm = TRUE)

# Create sequence of MrVBF values
newMrVBF <- seq(min(dat3$MrVBF, na.rm = TRUE),
                max(dat3$MrVBF, na.rm = TRUE),
                length.out = 9)

# Create newdata with MrVBF varying, other predictors at mean
newdata <- data.frame(
  Distance_to_Eucalypt_canopy.m. = mean(dat3$Distance_to_Eucalypt_canopy.m., na.rm = TRUE),
  PET = mean(dat3$PET, na.rm = TRUE),
  MrVBF = newMrVBF
)

# Predict on response scale
pred_link <- predict(model2, newdata = newdata, type = "link", se.fit = TRUE)

# Compute 95% CI on response scale
newdata$predicted <- exp(pred_link$fit)
newdata$lower <- exp(pred_link$fit - 1.96*pred_link$se.fit)
newdata$upper <- exp(pred_link$fit + 1.96*pred_link$se.fit)

# Plot
ggplot() +
  geom_jitter(data = dat3, aes(x = MrVBF, y = total_seedlings),
              width = 0.1, alpha = 0.6, color = "grey30") +
  geom_line(data = newdata, aes(x = MrVBF, y = predicted), color = "black", size = 1.2) +
  geom_ribbon(data = newdata, aes(x = MrVBF, ymin = lower, ymax = upper),
              fill = "brown", alpha = 0.4) +
  labs(x = "Flatness Index (MrVBF)", y = "Total seedlings") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )

