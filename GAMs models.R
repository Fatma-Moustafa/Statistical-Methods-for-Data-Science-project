########
# GAMs #
########
library(mgcv)
library(PerformanceAnalytics)

# read dataset and exclude columns of date, region name and code, state, long, lat, ...
train_set <- read.csv("G:/SMDS/project/00. Exploration/csv/train_set.csv")
train_data <- subset(train_set, select = -c(1, 2, 3, 4, 5, 6, 19))
test_set <- read.csv("G:/SMDS/project/00. Exploration/csv/test_set.csv")
test_data <- subset(test_set, select = -c(1, 2, 3, 4, 5, 6, 19))


gam.full  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(positive_variation)+s(new_positive)+s(total_people_tested)+s(tests_performed), data = train_data)
# Diagnostic plot:
plot(gam.full, residuals = TRUE, pch = 19, pages = 3)
par(mfrow=c(2,2))
gam.check(gam.full)
AIC(gam.full)
summary(gam.full)
concurvity(gam.full)
# prediction of the upcoming 14 days
predfull=predict(gam.full, newdata= test_data,type="link")
predfull
Rfull=sqrt(mean((test_data$patients_intensive_care -as.integer( predfull))^2))
Rfull
predictedfull<-cbind(c(test_data$patients_intensive_care) , c(predfull))


gam.1 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(new_positive)+s(total_people_tested)+(tests_performed+ positive_variation), data = train_data) 
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=10, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.1)
concurvity(gam.1)
AIC(gam.1)
summary(gam.1)
# prediction of the upcoming 14 days
pred1=predict(gam.1, newdata= test_data,type="link")
R1=sqrt(mean((test_data$patients_intensive_care - as.integer(pred1))^2))
predicted1<-cbind(c(test_data$patients_intensive_care) , c(pred1))
R1

gam.2 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(total_people_tested)+(tests_performed), data = train_data) 
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.2,residuals=TRUE,pch=10, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.2)
AIC(gam.2)
concurvity(gam.2)
summary(gam.2)
# prediction of the upcoming 14 days
pred2=predict(gam.2, newdata= test_data,type="link")
R2=sqrt(mean((test_data$patients_intensive_care - as.integer(pred2))^2))
R2

gam.3 <- gam(patients_intensive_care ~s(total_patients_hospitalized) + s(recovered)+ s(death)+  s(total_cases)+s(total_people_tested)+(tests_performed), data = train_data) 
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.3,residuals=TRUE,pch=10, pages=1)
par(mfrow=c(2,2))
gam.check(gam.3)
AIC(gam.3)
concurvity(gam.3)
summary(gam.3)
# prediction of the upcoming 14 days
pred3=predict(gam.3, newdata= test_data,type="link")
R3=sqrt(mean((test_data$patients_intensive_care - as.integer(pred3))^2))
R3

gam.4 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s( death)+ s(positive)+s(total_people_tested)+(positive_variation), data = train_data) 
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.4,residuals=TRUE,pch=10, pages=1)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
concurvity(gam.4)
summary(gam.4)
# prediction of the upcoming 14 days
pred4=predict(gam.4, newdata= test_set,type="link")
R4=sqrt(mean((test_data$patients_intensive_care - as.integer(pred4))^2))
R4

gam.5 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s( death)+ s(positive), data = train_data)  
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.5,residuals=TRUE,pch=10, pages=1)
par(mfrow=c(2,2))
gam.check(gam.5)
AIC(gam.5)
concurvity(gam.5)
summary(gam.5)
# prediction of the upcoming 14 days
pred5=predict(gam.5, newdata= test_set,type="link")
R5=sqrt(mean((test_data$patients_intensive_care - as.integer(pred5))^2))
R5

gam.6 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s( death), data = train_data)
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.6,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.6)
AIC(gam.6)
summary(gam.6)
# prediction of the upcoming 14 days
pred6=predict(gam.6, newdata= test_set,type="link")
R6=sqrt(mean((test_data$patients_intensive_care - as.integer(pred6))^2))
R6
