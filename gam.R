library(mgcv)
library(PerformanceAnalytics)

chart.Correlation(df[7:19])

glm.1 <- glm(patients_intensive_care ~ death, data = train_set, family = Gamma(link = "log")) 
gam.1 <- gam(patients_intensive_care ~ s(death), data = train_set, family = Gamma(link = "log")) 

glm.2 <- glm(patients_intensive_care ~ death + recovered, data = train_set, family = Gamma(link = "log")) 
gam.2 <- gam(patients_intensive_care ~ s(death) + s(recovered), data = train_set, family = Gamma(link = "log")) 

AIC(glm.1, gam.1, glm.2, gam.2) 


