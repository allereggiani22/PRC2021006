source(here('R', 'librerie.R'))


# Analisi geolocalizzazione -----------------------------------------------

dt2022 <- read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                     sheet = "CAMPIONI PRC COVID 2022", col_types = c("numeric", 
                                                                      "skip", "text", "skip", "text", 
                                                                      "text", "skip", "text", "text", "date", 
                                                                      "text", "date", "text", "skip", "text", 
                                                                      "skip"))

dt2023 <- read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                     sheet = "CAMPIONI PRC COVID 2023", col_types = c("numeric", 
                                                                      "skip", "text", "skip", "text", 
                                                                      "text", "text", "text", "date", 
                                                                      "text", "date", "text", "skip", "text", 
                                                                      "skip"))

dt2024 <- read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                     sheet = "CAMPIONI PRC COVID 2024", col_types = c("numeric", 
                                                                      "skip", "skip", "text", "skip", "text", 
                                                                      "text", "text", "text", "date", 
                                                                      "text", "date", "text", "skip", "text", 
                                                                      "skip"))


dt2023 <- add_column(dt2023,Anno= 2023, .after="Materiale")
dt2022 <- add_column(dt2022,Anno= 2022, .after="Materiale")
dt2024 <- add_column(dt2024,Anno= 2024, .after="Materiale")
dt2022 <- dt2022[-(3885:4005),]

dati <- bind_rows(dt2022,dt2023,dt2024)

dati <- clean_names(dati)
#view(dati)


unique(dati$specie)
dati <- dati %>% 
  mutate(specie = replace(specie, specie %in% "SCOTTAIOLO", "SCOIATTOLO"))

dati <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf))

dati <- 
  dati %>% mutate(sacco= ifelse(sacco =="T", 0, as.numeric(sacco)))

dt_an <-   dati %>%
  filter(!is.na(materiale)) %>% 
  filter(!is.na(progr) | materiale =="POOL") %>% 
  filter(!is.na(specie)) %>% 
  filter(specie != "CINGHIALE") %>% 
  filter(materiale != "MILZA") 


unique(dt_an$materiale)


dt_an <- dt_an %>% 
  mutate(materiale = replace(materiale, materiale %in% "T. NAS", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. RETT", "T.RETT"),
         materiale = replace(materiale, materiale %in% c("ILEO/DIGIUNO", "DIGIUNO","DUODENO"), "INTESTINO"),
         materiale = replace(materiale, materiale %in% "SENI NASALI", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. FAR", "FARINGE"),
         materiale = replace(materiale, materiale %in% "TR + PO", "TRACHEA + POLMONE"))#,
# specie = replace(specie, specie %in% "CAPRIOLO", "ROE DEER"),
# specie = replace(specie, specie %in% "DAINO", "FALLOW DEER"),
# specie = replace(specie, specie %in% "TASSO", "BADGER"),
# specie = replace(specie, specie %in% "ISTRICE", "PORCUPINE"),
# specie = replace(specie, specie %in% "LEPRE", "HARE"),
# specie = replace(specie, specie %in% "RICCIO", "HEDGEHOG"),
# specie = replace(specie, specie %in% "SCOIATTOLO", "SQUIRREL"),
# specie = replace(specie, specie %in% "LUPO", "WOLF"),
# specie = replace(specie, specie %in% "CERVO", "DEER"),
# specie = replace(specie, specie %in% "FAINA", "BEECH MARTEN"),
# specie = replace(specie, specie %in% "DELFINO", "TURSIOPS"),
# specie = replace(specie, specie %in% "VOLPE", "RED FOX"),
# specie = replace(specie, specie %in% "SILVILAGO", "MARSH RABBIT"),
# specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
# specie = replace(specie, specie %in% "CANGURO", "WALLABY"),
# specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
# specie = replace(specie, specie %in% "CONIGLIO", "RABBIT"),
# specie = replace(specie, specie %in% "LONTRA", "OTTER"),
# specie = replace(specie, specie %in% "SURICATO", "MEERKAT"),
# specie = replace(specie, specie %in% "AGUTI DI AZARA", "AZARA'S AGOUTI"),
# specie = replace(specie, specie %in% "RATTO", "RAT"),
# specie = replace(specie, specie %in% "TOPO", "MOUSE"),
# specie = replace(specie, specie %in% "GHIRO", "DORMOUSE"),
# specie = replace(specie, specie %in% "PROCIONE", "RACCOON"),
# specie = replace(specie, specie %in% "MARTORA", "PINE MARTEN"),
# specie = replace(specie, specie %in% "FURETTO", "FERRET"),
# specie = replace(specie, specie %in% "DONNOLA", "WEASEL"),
# specie = replace(specie, specie %in% "TALPA", "MOLE"),
# specie = replace(specie, specie %in% "SCIACALLO", "JACKAL"),
# specie = replace(specie, specie %in% "CINCILLA'", "CHINCHILLA"),
# specie = replace(specie, specie %in% "PIPISTRELLO", "BAT"),
# specie = replace(specie, specie %in% "TURSIOPE", "TURSIOPS"))

#voglio raggruppare l'elenco dei campioni analizzati per pancov per tenere solo conferimento, anno, specie,
#in modo da poter poi agganciare la tabella bobj con la geolocalizzazione


dati_geo <- dt_an %>% group_by(conf_orig, specie, anno) %>% summarise()

bobj <- read_excel("dati/estrazione bobj.xlsx", 
                   col_types = c("skip", "skip", "numeric", 
                                 "date", "numeric", "numeric", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text"))


view(bobj)

bobj_u <- unique(bobj) #per rimuovere le righe duplicate
view(bobj_u)

bobj_u <- clean_names(bobj_u)

bobj_s <- bobj_u %>% select(-c(data_reg, n_campioni, riserva, sesso))

library(dplyr)

view(dati_geo)

dati_geo <- dati_geo %>% separate(conf_orig, into = c("conferimento", "n_campione"), sep = "/") %>%
   mutate(n_campione = replace_na(n_campione, "1")) %>% 
   dplyr::rename(anno_reg = anno) %>% 
  mutate(conferimento= as.numeric(conferimento))

dati_integrati <- 
  dati_geo %>% 
  left_join(bobj_s, by= c("conferimento", "anno_reg"))

di <- dati_integrati %>% select(- specie.y) %>% 
  dplyr::rename(specie = specie.x)


write.xlsx(di, file = "./exports/dati integrati x geotag.xlsx")

#ora devo cercare di recuperare dalla colonna note tutte le info utili possibili

library(stringr)

library(dplyr)
library(stringr)

estrai_info_note <- function(df, colonna_note = "note") {
  # Testo di partenza
  note_text <- df[[colonna_note]]
  
  # Pattern per coordinate X e Y (accetta anche à, spazi strani)
  coord_x_pattern <- "(?i)x[:=]?\\s*(\\d{2,3})[°à]?\\s*(\\d{1,2})'?\\s*(\\d{1,2})?''?"
  coord_y_pattern <- "(?i)y[:=]?\\s*(\\d{2,3})[°à]?\\s*(\\d{1,2})'?\\s*(\\d{1,2})?''?"
  
  # Pattern per toponimi, includendo maschile/femminile e locuzioni comuni
  luogo_pattern <- paste0(
    "(?i)(proveniente|provenienta|comune di|in loc\\.?|loc\\.?|in via|via|rinvenuto|rinvenuta|ritrovato|ritrovata|recuperato|recuperata|presso)[:\\s]+",
    "([\\w'\\.\\sàèòùì,\\-]+)"
  )
  
  # Funzione per convertire coordinate in decimali
  converti_coord <- function(gradi, primi, secondi) {
    gradi <- as.numeric(gradi)
    primi <- as.numeric(primi)
    secondi <- as.numeric(secondi)
    secondi[is.na(secondi)] <- 0
    primi[is.na(primi)] <- 0
    gradi + primi / 60 + secondi / 3600
  }
  
  # Estrai e converti coordinate
  x_match <- str_match(note_text, coord_x_pattern)
  y_match <- str_match(note_text, coord_y_pattern)
  
  coord_x_dec <- converti_coord(x_match[,2], x_match[,3], x_match[,4])
  coord_y_dec <- converti_coord(y_match[,2], y_match[,3], y_match[,4])
  
  # Estrai luogo
  luogo_match <- str_match(note_text, luogo_pattern)[,3] %>% str_trim()
  
  # Aggiungi colonne al df originale
  df <- df %>%
    mutate(
      coord_x_raw = x_match[,1],
      coord_y_raw = y_match[,1],
      coord_x = coord_x_dec,
      coord_y = coord_y_dec,
      luogo = luogo_match
    )
  
  return(df)
}

estratti <- estrai_info_note(di)
view(estratti)

#migliorabile, provo una seconda versione

estrai_info_note_v2 <- function(df, colonna_note = "note") {
  note_text <- df[[colonna_note]]
  
  # Pattern per coordinate (ripristinato la versione precedente)
  coord_x_pattern <- "X[:\\s]*([0-9]{1,3})[°\\s]*([0-9]{1,2})'?\\s*([0-9]{1,2})?''?"
  coord_y_pattern <- "Y[:\\s]*([0-9]{1,3})[°\\s]*([0-9]{1,2})'?\\s*([0-9]{1,2})?''?"
  
  # Pattern per luoghi - ottimizzato per gestire abbreviazioni e punti
  luogo_pattern <- paste0(
    "(?i)(provenient[ea] da[:\\s]+[^\\.\\-\\n]+|",
    "comune di[:\\s]+[^\\.\\-\\n]+|",
    "in loc[\\.\\s]*[:\\s]+[^\\.\\-\\n]+|",  # Permette 'loc.' seguito da spazio e da un nome
    "loc[\\.\\s]*[:\\s]+[^\\.\\-\\n]+|",     # Permette 'loc.' seguito da spazio e da un nome
    "in via[:\\s]+[^\\.\\-\\n]+|",
    "via[:\\s]+[^\\.\\-\\n]+|",
    "rinvenut[oa][:\\s]+[^\\.\\-\\n]+|",
    "ritrovat[oa][:\\s]+[^\\.\\-\\n]+|",
    "recuperat[oa][:\\s]+[^\\.\\-\\n]+|",
    "presso[:\\s]+[^\\.\\-\\n]+)"
  )
  
  # Funzione per convertire coordinate in decimali
  converti_coord <- function(gradi, primi, secondi) {
    gradi <- as.numeric(gradi)
    primi <- as.numeric(primi)
    secondi <- as.numeric(secondi)
    secondi[is.na(secondi)] <- 0
    primi[is.na(primi)] <- 0
    gradi + primi / 60 + secondi / 3600
  }
  
  # Estrazione coordinate
  x_match <- str_match(note_text, coord_x_pattern)
  y_match <- str_match(note_text, coord_y_pattern)
  coord_x_dec <- converti_coord(x_match[,2], x_match[,3], x_match[,4])
  coord_y_dec <- converti_coord(y_match[,2], y_match[,3], y_match[,4])
  
  # Estrazione luogo
  luogo_raw <- str_match(note_text, luogo_pattern)[,2] %>% str_trim()
  luogo_clean <- luogo_raw %>%
    str_remove_all("[\\.,;\\s]+$") %>%
    str_remove_all("^[\\.,;\\s]+")
  
  # Aggiunta colonne
  df <- df %>%
    mutate(
      coord_x_raw = x_match[,1],
      coord_y_raw = y_match[,1],
      coord_x = coord_x_dec,
      coord_y = coord_y_dec,
      luogo = luogo_clean
    )
  
  return(df)
}



estratti2 <- estrai_info_note_v2(di)
view(estratti2)

write.xlsx(estratti2, file = "./exports/dati estratti lavorati.xlsx")






