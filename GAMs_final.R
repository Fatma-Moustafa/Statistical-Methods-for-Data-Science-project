########
# GAMs #
########
library(mgcv)
library(car)
library(PerformanceAnalytics)

# read dataset and exclude columns of date, region name and code, state, long, lat, ...
train_set <- read.csv("G:/SMDS/project/00. Exploration/csv/train_set.csv")
train_data <- subset(train_set, select = -c(1, 2, 3, 4, 5, 6, 19))
test_set <- read.csv("G:/SMDS/project/00. Exploration/csv/test_set.csv")
test_data <- subset(test_set, select = -c(1, 2, 3, 4, 5, 6, 19))
train_data$color <- as.factor(train_data$color)
test_data$color <- as.factor(test_data$color)
train_data$color <- as.numeric(train_data$color)
test_data$color <- as.numeric(test_data$color)


gam.full  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(positive_variation)+s(new_positive)+s(total_people_tested)+s(tests_performed)+(color), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot:
plot(gam.full, residuals = TRUE, pch = 1, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.full)
AIC(gam.full)
vif(gam.full)
summary(gam.full)
# prediction of the upcoming 14 days
predfull=predict(gam.full, newdata= test_data,type="response")
Rfull=sqrt(mean((test_data$patients_intensive_care - predfull)^2))
concurvity(gam.full)

## linearize: home_confinement, recovered, positive_variation, new_positive, total_people_tested, tests_performed
gam.1 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+s(positive)+  s(total_cases)+ (home_confinement+recovered+positive_variation+new_positive+total_people_tested+tests_performed+color), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.1)
AIC(gam.1)
summary(gam.1)
# prediction of the upcoming 14 days
pred1=predict(gam.1, newdata= test_data,type="response")
R1=sqrt(mean((test_data$patients_intensive_care - pred1)^2))

# remove least significant variables: new_positive, positive_variation, total_cases
gam.2 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+s(positive)+ (color+home_confinement+recovered+total_people_tested+tests_performed), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.2,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.2)
AIC(gam.2)
summary(gam.2)
# prediction of the upcoming 14 days
pred2=predict(gam.2, newdata= test_data,type="response")
R2=sqrt(mean((test_data$patients_intensive_care - pred2)^2))

# remove least significant variables: positive, home_confinement, recovered
gam.3 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+ (total_people_tested+tests_performed+color), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.3,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.3)
AIC(gam.3)
summary(gam.3)
# prediction of the upcoming 14 days
pred3=predict(gam.3, newdata= test_data,type="response")
R3=sqrt(mean((test_data$patients_intensive_care - pred3)^2))
concurvity(gam.3)

#change number of basis functions
gam.4 <- gam(patients_intensive_care ~s(total_patients_hospitalized, k=12)+s(death, k=12)+ (total_people_tested+tests_performed+color), data = train_data,family = poisson(link= "log"), method="REML")
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.4,residuals=TRUE,pch=1, pages=1)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
summary(gam.4)
# prediction of the upcoming 14 days
pred4=predict(gam.4, newdata= test_set,type="response")
R4=sqrt(mean((test_data$patients_intensive_care - pred4)^2))

#gam.full Modified
#linearize home confinement, recovered, total-people_tested, tests_performed
#remove least significant variables: positive, total_cases, positive_variation, new_positive
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

