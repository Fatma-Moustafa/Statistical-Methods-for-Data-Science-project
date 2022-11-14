library(tidyverse)

theme_set(theme_bw())

# Patients Hospitalized
ggplot(df, aes(x = patients_hospitalized, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Total Patients Hospitalized
ggplot(df, aes(x = total_patients_hospitalized, y = patients_intensive_care)) + 
  geom_point(colour = "blue", size = 1)

# Home Confinement
ggplot(df, aes(x = home_confinement, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Positive
ggplot(df, aes(x = positive, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Positive Variation
ggplot(df, aes(x = positive_variation, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# New positive
ggplot(df, aes(x = new_positive, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Recovered
ggplot(df, aes(x = recovered, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Death
ggplot(df, aes(x = death, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Total Cases
ggplot(df, aes(x = total_cases, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Tests Performed
ggplot(df, aes(x = tests_performed, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

## NEXT TWO CONTAINS MISSING VALUES

# Total people Tested
ggplot(df, aes(x = total_people_tested, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)

# Death
ggplot(df, aes(x = intensive_care_admission, y = patients_intensive_care)) + 
  geom_point(colour = "#43d198", size = 1)





