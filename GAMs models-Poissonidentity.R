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

gam.full  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(positive_variation)+s(new_positive)+s(total_people_tested)+s(tests_performed), data = train_data,family = poisson(link= "identity"))
# Diagnostic plot:
plot(gam.full, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.full)
AIC(gam.full)
summary(gam.full)
# prediction of the upcoming 14 days
predfull=predict(gam.full, newdata= test_data,type="link",se.fit=TRUE)
Rfull=sqrt(mean((test_data$patients_intensive_care - predfull$fit)^2))

## linear: positive variation and test performed
gam.1 <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+s(positive)+  s(total_cases)+(positive_variation+new_positive+total_people_tested +home_confinement + recovered+tests_performed), data = train_data,family = poisson(link="identity"))

# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.1)
AIC(gam.1)
summary(gam.1)
# prediction of the upcoming 14 days
pred1=predict(gam.1, newdata= test_data,type="link",se.fit=TRUE)
R1=sqrt(mean((test_data$patients_intensive_care - pred1$fit)^2))

# remove positive variation
gam.2 <-gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+  s(total_cases)+(positive+positive_variation+new_positive+total_people_tested +home_confinement + recovered+tests_performed), data = train_data,family = poisson(link="identity"))
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.2,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.2)
AIC(gam.2)
summary(gam.2)
# prediction of the upcoming 14 days
pred2=predict(gam.2, newdata= test_data,type="link",se.fit=TRUE)
R2=sqrt(mean((test_data$patients_intensive_care - pred2$fit)^2))

#remove positive and home confinement
gam.3 <-gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+(positive+positive_variation+new_positive+total_people_tested +home_confinement + recovered+tests_performed), data = train_data,family = poisson(link="identity"))
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.3,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.3)
AIC(gam.3)
summary(gam.3)
# prediction of the upcoming 14 days
pred3=predict(gam.3, newdata= test_data,type="link",se.fit=TRUE)
R3=sqrt(mean((test_data$patients_intensive_care - pred3$fit)^2))

#remove total people tested
gam.4 <-gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+(positive+positive_variation+new_positive+total_people_tested +home_confinement +tests_performed), data = train_data,family = poisson(link="identity"))
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.4,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
summary(gam.4)
# prediction of the upcoming 14 days
pred4=predict(gam.4, newdata= test_set,type="link",se.fit=TRUE)
R4=sqrt(mean((test_data$patients_intensive_care - pred4$fit)^2))

#remove total cases
gam.5 <-gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(death)+(positive+home_confinement ), data = train_data,family = poisson(link="identity"))
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.5,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.5)
AIC(gam.5)
summary(gam.5)
# prediction of the upcoming 14 days
pred5=predict(gam.5, newdata= test_set,type="link",se.fit=TRUE)
R5=sqrt(mean((test_data$patients_intensive_care - pred5$fit)^2))

