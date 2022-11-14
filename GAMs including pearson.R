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

gam.0 <-gam(patients_intensive_care ~s(total_patients_hospitalized)
            + s(death)
            +(total_people_tested)+(tests_performed), 
            data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.0,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.0)
AIC(gam.0)
summary(gam.0)
# prediction of the upcoming 14 days
pred0=predict(gam.0, newdata= test_set,type="response")
RMSE0 <- rmse(test_set$patients_intensive_care,pred0)
concurvity(gam.0)

gam.1 <-gam(patients_intensive_care ~s(total_patients_hospitalized)
            +s(total_people_tested)+s(new_positive)+(color), 
            data = train_set,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.1)
AIC(gam.1)
summary(gam.1)
# prediction of the upcoming 14 days
pred1=predict(gam.1, newdata= test_set,type="response")
RMSE1 <- rmse(test_set$patients_intensive_care,pred1)
concurvity(gam.1)

gam.2 <-gam(patients_intensive_care ~s(total_patients_hospitalized)
            +s(total_people_tested), 
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

test_set$patients_intensive_care

### plot actual prediction of train set to spot overfitting
x <- seq(1, 138, by = 1)
y1 <- c(train_set$patients_intensive_care,test_set$patients_intensive_care) 
y2 <- c(predict(gam.2,type="response"), pred2)

pred.df <- data.frame(x, y1, y2)
ggplot(pred.df, aes(x = x)) +
  geom_line(aes(y = y1, colour = "Actual"), size = 0.5) +
  geom_line(aes(y = y2, colour = "Predicted"), size = 0.5) +
  scale_colour_manual("", breaks = c("Actual", "Predicted"),
                      values = c("blue", "red")) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
  scale_x_continuous(breaks = c(0,  10,  20, 30, 40, 50,  60, 70, 80,  90, 100,  110, 120, 130,140)) +
  xlab("Days") +
  ylab("Patients in intensive care") +
  geom_vline(xintercept = 124)+
  geom_vline(xintercept = 138)+
  theme_light()
