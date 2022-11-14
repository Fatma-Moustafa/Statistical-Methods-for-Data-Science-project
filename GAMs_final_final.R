########
# GAMs #
########
library(mgcv)
library(car)
library(Metrics)
library(PerformanceAnalytics)
library(ggplot2)

# read dataset and exclude columns of date, region name and code, state, long, lat, ...
train_set <- read.csv("G:/SMDS/project/train_set.csv")
test_set <- read.csv("G:/SMDS/project/test_set.csv")
train_set$color <- as.factor(train_set$color)
test_set$color <- as.factor(test_set$color)
train_set$color <- as.numeric(train_set$color)
test_set$color <- as.numeric(test_set$color)

gam.0  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) 
              + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(positive_variation)
              +s(new_positive)+s(total_people_tested)+s(tests_performed)+(color), 
              data = train_set,family = poisson(link= "log"), method="REML")

# Diagnostic plot:
plot(gam.0, residuals = TRUE, pch = 1)
qq.gam(gam.0)
par(mfrow=c(2,2))
gam.check(gam.0)
AIC(gam.0)
summary(gam.0)
# prediction of the upcoming 14 days
pred0=predict(gam.0, newdata= test_set,type="response")
RMSE0 <- rmse(test_set$patients_intensive_care,pred0 )

gam.1 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ (home_confinement) 
             + (recovered)+ s(death)+(positive_variation)
             +(new_positive)+(total_people_tested)+(tests_performed)+(color), 
             data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.1)
AIC(gam.1)
summary(gam.1)
# prediction of the upcoming 14 days
pred1=predict(gam.1, newdata= test_set,type="response")
RMSE1 <- rmse(test_set$patients_intensive_care,pred1 )

gam.2 <-gam(patients_intensive_care ~s(total_patients_hospitalized)
            + (home_confinement) + s(death)
            +(total_people_tested)+(tests_performed), 
            data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.2,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.2)
AIC(gam.2)
summary(gam.2)
# prediction of the upcoming 14 days
pred2=predict(gam.2, newdata= test_set,type="response")
RMSE2 <- rmse(test_set$patients_intensive_care,pred2)



gam.3 <-gam(patients_intensive_care ~s(total_patients_hospitalized)
             + s(death)
            +(total_people_tested)+(tests_performed), 
            data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.3,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.3)
AIC(gam.3)
summary(gam.3)
# prediction of the upcoming 14 days
pred3=predict(gam.3, newdata= test_set,type="response")
RMSE3 <- rmse(test_set$patients_intensive_care,pred3)
concurvity(gam.3)

gam.4 <-gam(patients_intensive_care ~
              s(total_patients_hospitalized,death)
            +(total_people_tested)+(tests_performed), 
            data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.4,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
summary(gam.4)
# prediction of the upcoming 14 days
pred4=predict(gam.4, newdata= test_set,type="response")
RMSE4 <- rmse(test_set$patients_intensive_care,pred4)
concurvity(gam.4)

