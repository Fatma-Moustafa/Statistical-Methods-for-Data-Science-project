########
# GAMs #
########
library(mgcv)
library(PerformanceAnalytics)

# read dataset and exclude columns of date, region name and code, state, long, lat, ...
train_set <- read.csv("G:/SMDS/project/00. Exploration/csv/train_set.csv")
train_data <- train_set[7:18]

#
gam.3 <- gam(patients_intensive_care ~ s(recovered, k=10, sp=0.001)+ (home_confinement)+s(total_patients_hospitalized), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.3, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.3)
summary(gam.3)

gam.33 <- gam(patients_intensive_care ~ s(recovered, total_patients_hospitalized)+ (home_confinement), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.33, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.33)
(gam.33)

#
gam.4 <- gam(patients_intensive_care ~ s(total_patients_hospitalized)+ (home_confinement) +s(recovered)+ (positive), data = train_data,
             family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.4, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
concurvity(gam.4)

gam.44 <- gam(patients_intensive_care ~ s(total_patients_hospitalized, recovered)+ (home_confinement) + (positive), data = train_data,
             family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.44, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.44)
AIC(gam.44)
concurvity(gam.44)
# 
gam.5 <- gam(patients_intensive_care ~  s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+(positive), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.5, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.5)
AIC(gam.5)
concurvity(gam.5)

gam.55 <- gam(patients_intensive_care ~  s(total_patients_hospitalized,death)+ (home_confinement) + (recovered)+(positive), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.55, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.55)
AIC(gam.55)
concurvity(gam.55)
#
gam.6 <- gam(patients_intensive_care ~ s(total_patients_hospitalized)+ (home_confinement) + (recovered)+s(death)+(positive)+ s(total_cases), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.6, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.6)
AIC(gam.6)


gam.66 <- gam(patients_intensive_care ~ s(total_patients_hospitalized,total_cases, death)+ (home_confinement) + (recovered)+(positive), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.66, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.66)
AIC(gam.66)

gam.666 <- gam(patients_intensive_care ~ s(total_patients_hospitalized)+ s(death,total_cases)+ (home_confinement) + (recovered)+(positive), data = train_data,
              family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.666, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.666)
AIC(gam.666)

gam.6666 <- gam(patients_intensive_care ~ s(total_patients_hospitalized,total_cases)+ s(death)+ (home_confinement) + (recovered)+(positive), data = train_data,
               family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.6666, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.6666)
AIC(gam.6666)

# Compare the fitted models by using AIC
AIC(gam.3,gam.33, gam.4,gam.44, gam.5, gam.55, gam.6,gam.66,gam.666,gam.6666)
'''   df      AIC
gam.3    16.02897 893.2402
gam.33   30.95742 998.7266
gam.4    16.02897 893.2402
gam.44   30.95744 998.7267
gam.5    16.20772 893.6034
gam.55   29.50473 916.0604
gam.6    16.20776 893.6035
gam.66   35.35451 941.2333
gam.666  16.17915 895.6645
gam.6666 31.46953 923.7347
'''

# prediction of the upcoming 14 days
test_set <- read.csv("G:/SMDS/project/00. Exploration/csv/test_set.csv")
test_data <- test_set[7:18]

pred3= predict(gam.3, newdata=test_data  )
pred4= predict(gam.4 )


p1=table(pred3, test_data$patients_intensive_care)
1-sum(diag(p1))/sum(p1)
##[1] 0.9285714

pred2= predict(gam.2, newdata=test_data )
p2= table(pred2, test_data$patients_intensive_care)
1-sum(diag(p2))/sum(p2)
##[1] 0.9285714

anova(gam.1, gam.2, gam.3, gam.4, gam.5, gam.6)

# prediction of the upcoming 14 days
test_set <- read.csv("G:/SMDS/project/00. Exploration/csv/test_set.csv")
test_data <- test_set[7:19]

predict(gam.1,newdata= test_set,type="link",se.fit=TRUE)

