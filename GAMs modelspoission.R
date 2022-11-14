########
# GAMs #
########
library(mgcv)
library(PerformanceAnalytics)

# read dataset and exclude columns of date, region name and code, state, long, lat, ...
train_set <- read.csv("G:/SMDS/project/00. Exploration/csv/train_set.csv")
train_data <- train_set[7:18]

# GAM model considering a single predictor(patients hospitalized)and the outcome (patients_intensive_care)  distributed according a Gamma 
gam.1 <- gam(patients_intensive_care ~ s(recovered, k=20), data = train_data,  family = poisson(link = "log")) 
# Diagnostic plot: represents the smooth function and the partial residuals
plot(gam.1,residuals=TRUE,pch=10)
par(mfrow=c(2,2))
gam.check(gam.1)
AIC(gam.1)
concurvity(gam.1)

# Adding HOME CONFINEMENT
gam.2 <- gam(patients_intensive_care ~ s(recovered)+ s(home_confinement), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.2, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.2)
AIC(gam.2)
concurvity(gam.2)

# Adding ReCOVERED
gam.3 <- gam(patients_intensive_care ~ s(recovered)+ s(home_confinement)+s(total_patients_hospitalized), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.3, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.3)
AIC(gam.3)
concurvity(gam.3)

#Adding DEATH
gam.4 <- gam(patients_intensive_care ~ s(total_patients_hospitalized)+ (home_confinement) +s(recovered)+ s(positive), data = train_data,
             family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.4, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.4)
AIC(gam.4)
concurvity(gam.4)

# Adding POSITIVE_VARIATION
gam.5 <- gam(patients_intensive_care ~  s(total_patients_hospitalized)+ (home_confinement) + s(recovered)+ s(death)+(positive), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.5, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.5)
AIC(gam.5)
concurvity(gam.5)

#Adding TOTAL_PEOPLE TESTED
gam.6 <- gam(patients_intensive_care ~ s(total_patients_hospitalized)+ (home_confinement) + (recovered)+s(death)+(positive)+ s(total_cases), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.6, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.6)
AIC(gam.6)

# Adding NEWPositive
gam.7 <- gam(patients_intensive_care ~  s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+(positive)+  s(total_cases)+s(positive_variation), data = train_data,
             family = poisson(link = "log"))
# Diagnostic plot:
plot(gam.7, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.7)
AIC(gam.7)

# Adding POSITIVE_VARIATION and remove Intensive care admission
gam.8  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+(positive)+  s(total_cases)+(positive_variation)+s(new_positive), data = train_data,family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.8, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.8)
AIC(gam.8)

gam.9  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+(positive)+  s(total_cases)+(positive_variation)+(new_positive)+s(total_people_tested), data = train_data,family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.9, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.9)
AIC(gam.9)

gam.10  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+(positive)+  s(total_cases)+(positive_variation)+(new_positive)+(total_people_tested)+s(tests_performed), data = train_data,family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.10, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.10)
AIC(gam.10)

gam.full  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ s(home_confinement) + s(recovered)+ s(death)+s(positive)+  s(total_cases)+s(positive_variation)+s(new_positive)+s(total_people_tested)+s(tests_performed), data = train_data,family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.full, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.full)
AIC(gam.full)

gam.11  <- gam(patients_intensive_care ~s(total_patients_hospitalized)+ (home_confinement) + (recovered)+ s(death)+s(positive)+  s(total_cases)+(total_people_tested)+(tests_performed), data = train_data,family =poisson(link = "log"))
# Diagnostic plot:
plot(gam.11, residuals = TRUE, pch = 19, pages = 1)
par(mfrow=c(2,2))
gam.check(gam.11)
summary(gam.11)
AIC(gam.11)

# Compare the fitted models by using AIC
AIC(gam.1, gam.2, gam.3, gam.4, gam.5, gam.6, gam.7, gam.8, gam.9, gam.10)
'''   df      AIC
gam.1   9.988984 2630.0227
gam.2  15.333561  934.1238
gam.3  16.028986  893.2403
gam.4  16.028986  893.2403
gam.5  16.207727  893.6035
gam.6  16.207758  893.6035
gam.7  17.267786  895.3821
gam.8  18.229967  897.3263
gam.9  18.896452  897.8325
gam.10 19.448849  899.1418
'''


