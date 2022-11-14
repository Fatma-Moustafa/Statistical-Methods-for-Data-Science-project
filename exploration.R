library(tidyverse)

data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

df <- data %>% 
        filter(denominazione_regione == "Emilia-Romagna") %>%
        select(-16, -17, -21, -23, -24, -25, -26, -27, -28, -29, -30) %>%
        rename(
          date = data,
          state = stato,
          region_code = codice_regione,
          region_name = denominazione_regione,
          patients_hospitalized = ricoverati_con_sintomi,
          patients_intensive_care = terapia_intensiva,
          total_patients_hospitalized = totale_ospedalizzati,
          home_confinement = isolamento_domiciliare,
          positive = totale_positivi,
          positive_variation = variazione_totale_positivi,
          new_positive = nuovi_positivi,
          recovered = dimessi_guariti,
          death = deceduti,
          total_cases = totale_casi,
          tests_performed = tamponi,
          total_people_tested = casi_testati,
          intensive_care_admission = ingressi_terapia_intensiva
        )

write.csv(df, "./covid.csv", row.names = TRUE)
