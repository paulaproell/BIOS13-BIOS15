#Exercise variance in mixed models

#load packages
install.packages("glmmTMB")
library(glmmTMB)

#make data set
set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

#make a plot 
plot(x1, y, col=as.numeric(groupID), las=1)

#sort data
data = data.frame(y, x1, groupID)
head(data)

#fit model
m = glmmTMB(y ~ 1 + (1|groupID), data=data)
summary(m)

#Analyse variances
VarCorr(m)

VarAmongGroups <- attr(VarCorr(m)$cond$groupID, "stddev")^2   
VarWithinGroups <- attr(VarCorr(m)$cond, "sc")^2

VarAmongGroups     #9.204914 
var(groupmeans)    #9.805633

#average sampling variance
mean_sampling_variance = mean(tapply(y, groupID, var)/20)
var(groupmeans) - mean_sampling_variance     #9.212205

#variance explained by model
VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100

#CV and CV^2 of variances
CV2_Among = VarAmongGroups/mean(y)^2
CV2_Within = VarWithinGroups/mean(y)^2
CV2_Total = CV2_Among + CV2_Within

#make a table
df = data.frame(Mean = mean(x1), SD = sd(x1),
                Among = VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100,
                Within = VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, MARGIN=2, FUN=round, digits=2)
df


#Random intercept regression
m = glmmTMB(y ~ x1 + (1|groupID), data=data)
summary(m)
coef(m)
newx = seq(min(x1), max(x1), length.out=200)
plot(x1, y, las=1)
for(i in 1:length(levels(groupID))){
  y_hat = coef(m)$cond$groupID[i,1] + coef(m)$cond$groupID[i,2]*newx
  lines(newx, y_hat, col=i)
}
y_hat = predict(m, newdata=list(x1=newx, groupID=rep("Group5",200)), re.form=NULL)

#fit lm
lM <- lm(y ~ x1)
summary(lM)
lm1 <- 25.192 + 1.321 * x1
plot(lm1,ylim=c(0,50))








