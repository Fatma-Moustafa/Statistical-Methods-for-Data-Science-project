#GLM
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(caret) 
library(vip)
library(mlbench)
library(tidyverse)
library(ggplot2)
library(car)
library(mgcv)
library(PerformanceAnalytics)
set.seed(1234)
############
#setup data#
############
#full data set
train_set <- read.csv("G:/SMDS/project/00. Exploration/csv/train_set.csv")
train_data <- subset(train_set, select = -c(1, 2, 3, 4, 5, 6, 19))
test_set <- read.csv("G:/SMDS/project/00. Exploration/csv/test_set.csv")
test_data <- subset(test_set, select = -c(1, 2, 3, 4, 5, 6, 19))

#color as numeric
train_data$color <- as.factor(train_data$color)
test_data$color <- as.factor(test_data$color)
train_data$color <- as.numeric(train_data$color)
test_data$color <- as.numeric(test_data$color)
str(train_data)

##LM best model
lm1<- lm(patients_intensive_care ~patients_hospitalized+ recovered+ new_positive + color, data= train_data)
summary(lm1)
AIC(lm1)
par(mfrow=c(2,2))
plot(lm1)
pred1=predict(lm1, newdata= test_data, type= "response")
rmse1 <- RMSE(pred = pred1, obs = test_data$patients_intensive_care)
#compare to glm poisson
glm1<- glm(patients_intensive_care ~patients_hospitalized+ recovered+ new_positive + color, data= train_data,family = poisson)
summary(glm1)
par(mfrow=c(2,2))
plot(glm1)
pred11=predict(glm1, newdata= test_data, type= "response")
rmse11 <- RMSE(pred = pred11, obs = test_data$patients_intensive_care)
#compare to GAMS
gam1<- gam(patients_intensive_care ~s(patients_hospitalized)+ s(recovered)+ (new_positive) + (color), data= train_data,family = poisson)
summary(gam1)
par(mfrow=c(2,2))
plot(gam1,residuals=TRUE,pch=1, pages=1)
pred111=predict(gam1, newdata= test_data, type= "response")
rmse111 <- RMSE(pred = pred111, obs = test_data$patients_intensive_care)
## the lm is the best in terms of RMSE then the gams with smoothing patients hospitalized and recovered variables


##LM best model with interactions
lm2 <- lm(patients_intensive_care ~ patients_hospitalized + new_positive 
          + recovered:color + death:recovered, data = train_data)
summary(lm2)
AIC(lm2)
par(mfrow=c(2,2))
plot(lm2)
pred2=predict(lm2, newdata= test_data, type= "response")
rmse2 <- RMSE(pred = pred2, obs = test_data$patients_intensive_care)
#compare to glm poisson
glm2<- glm(patients_intensive_care ~ patients_hospitalized + new_positive 
           + recovered:color + death:recovered, data= train_data,family = poisson)
summary(glm2)
par(mfrow=c(2,2))
plot(glm2)
pred22=predict(glm2, newdata= test_data, type= "response")
rmse22 <- RMSE(pred = pred22, obs = test_data$patients_intensive_care)
#compare to GAMS
gam2<- gam(patients_intensive_care ~ s(patients_hospitalized)  + s(new_positive) 
           + s(recovered,color) + s(death,recovered), data= train_data,family = poisson)
summary(gam2)
AIC(gam2)
par(mfrow=c(2,2))
plot(gam2,residuals=TRUE,pch=1, pages=1)
pred222=predict(gam2, newdata= test_data, type= "response")
rmse222 <- RMSE(pred = pred222, obs = test_data$patients_intensive_care)
## the glm is the best in terms of RMSE when interactions included


##################################
##BEST MODEL GLM##
fit3 <- glm(patients_intensive_care ~patients_hospitalized+total_patients_hospitalized+ home_confinement+ recovered+ death+total_people_tested+tests_performed, data= train_data,family = poisson)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
pred3=predict(fit3, newdata= test_data, type= "response")
rmse3 <- RMSE(pred = pred3, obs = test_data$patients_intensive_care)
##COMPARE TO GAM##
gam4 <- gam(patients_intensive_care ~s(patients_hospitalized)+s(total_patients_hospitalized)+ s(home_confinement)+ s(recovered)+ s(death)+s(total_people_tested)+s(tests_performed), data= train_data,family = poisson(link= "log"), method="REML")
plot(gam4,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam4)
AIC(gam4)
summary(gam4)
pred4=predict(gam4, newdata= test_set,type="response")
Rg4=sqrt(mean((test_data$patients_intensive_care - pred4)^2))
##lower AIC compared to the glm using the same variables and lower rmse

###########################################
##GAM best model##
gam.5 <- gam(patients_intensive_care ~s(total_patients_hospitalized, k=12)+s(death, k=12)+ (home_confinement+recovered+total_people_tested+tests_performed+color), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.5,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.5)
AIC(gam.5)
summary(gam.5)
# prediction of the upcoming 14 days
pred5=predict(gam.5, newdata= test_set,type="response")
R5=sqrt(mean((test_data$patients_intensive_care - pred5)^2))

##compare to glm##
glm.5 <- glm(patients_intensive_care ~total_patients_hospitalized+death+ home_confinement+recovered+total_people_tested+tests_performed+color, data = train_data,family = poisson)
summary(glm.5)
par(mfrow=c(2,2))
plot(glm.5)
pred.5=predict(glm.5, newdata= test_data, type= "response")
rmse.5<- RMSE(pred = pred.5, obs = test_data$patients_intensive_care)
##glm has worst results for the same set of variables
