source(here('R', 'librerie.R'))
library(zoo)

dt2023 <- read_excel("dati/25032024 Elenco Campioni PRC 2021_006.xlsx", 
                     sheet = "CAMPIONI PRC COVID 2023", col_types = c("skip", 
                                                                "skip", "text", "skip", "text", 
                                                                "text", "text", "text", "skip", "skip", 
                                                                "skip", "skip", "skip", "skip", "skip"))

dt2024 <- read_excel("dati/25032024 Elenco Campioni PRC 2021_006.xlsx", 
                     sheet = "CAMPIONI PRC COVID 2024", col_types = c("skip", 
                                                                      "skip", "text", "skip", "text", 
                                                                      "text", "text", "text", "skip", "skip", 
                                                                      "skip", "skip", "skip", "skip", "skip"))
dati <- bind_rows(dt2023, dt2024)

dati <- clean_names(dati)

dati <- dati %>% 
  mutate(across(c("conf_orig","specie","provenienza","sacco"), na.locf))

export <- dati %>% 
  mutate(materiale = replace(materiale, materiale %in% "SIEROLOGIA", "SIERO"),
         materiale = replace(materiale, materiale %in% "CUORE", "SIERO"),
         materiale = replace(materiale, materiale %in% "COAGULO CARDIACO", "SIERO"),
         sacco = as.numeric(sacco)) %>% 
  filter(materiale %in% c("SIERO","UMOR","MUSCOLO")) %>%
  filter(sacco > 125)

export %>% write.xlsx(.,"exports/Elenco sieri dal sacco 126.xlsx")
