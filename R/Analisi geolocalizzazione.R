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

# Import Bobj -------------------------------------------------------------


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

# Estrazione dati toponimi e/o coordinate dal campo note Bobj -------------------------------------


#ora devo cercare di recuperare dalla colonna note tutte le info utili possibili



library(stringr)


estrai_info_note <- function(df, colonna_note = "note") {
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

estratti <- estrai_info_note(di)
view(estratti)

write.xlsx(estratti, file = "./exports/dati estratti lavorati.xlsx")

# Ottenimento coordinate per conferimenti, con OSM --------


#ho lavorato un po' a mano sul file degli estratti per pulirlo, la funzione non è perfetta

#ora ricarico i dati lavorati e rifaccio la conversione di coordinate in decimali

dati_aggiustati <- read_excel("dati/dati estratti lavorati corretti a mano.xlsx")
view(dati_aggiustati)

#devo correggere le coordinate y di 2 luoghi che le hanno indicate in maniera scorretta, finendo in mare

dati_aggiustati <- dati_aggiustati %>% 
  mutate(coord_y_raw = replace(coord_y_raw, coord_y_raw %in% "Y 12°59'22''", "Y 11°59'22''"),
         coord_y_raw = replace(coord_y_raw, coord_y_raw %in% "Y:12°47'", "Y 12°04'14''"),
         coord_x_raw = replace(coord_x_raw, coord_x_raw %in% "X 48°08'18''", "X 44°08'18''"),
         coord_x_raw = replace(coord_x_raw, coord_x_raw %in% "X 46°17'19''", "X 44°47'19''"),
         coord_x_raw = replace(coord_x_raw, coord_x_raw %in% "X:44°57'24''", "X:43°57'24''"),
         coord_x_raw = replace(coord_x_raw, coord_x_raw %in% "X:44°08'32''", "X:44°04'59''"),
         coord_y_raw = replace(coord_y_raw, coord_y_raw %in% "Y12°29'01''", "Y12°17'24''"),
         coord_y_raw = replace(coord_y_raw, coord_y_raw %in% "Y 11°24'46''", "11°54'46''"))
                                 

converti_coordinate_decimali <- function(df, col_x_raw = "coord_x_raw", col_y_raw = "coord_y_raw") {
  
  # Funzione interna per estrarre gradi, primi, secondi da stringa tipo "44°12'20''"
  estrai_gps <- function(coord_str) {
    pattern <- "([0-9]{1,3})[°\\s]*([0-9]{1,2})'?\\s*([0-9]{1,2})?''?"
    match <- str_match(coord_str, pattern)
    gradi <- as.numeric(match[, 2])
    primi <- as.numeric(match[, 3])
    secondi <- as.numeric(match[, 4])
    secondi[is.na(secondi)] <- 0
    primi[is.na(primi)] <- 0
    return(gradi + primi / 60 + secondi / 3600)
  }
  
  # Calcolo decimali solo se le colonne raw esistono
  if (col_x_raw %in% names(df)) {
    df$coord_x <- estrai_gps(df[[col_x_raw]])
  }
  if (col_y_raw %in% names(df)) {
    df$coord_y <- estrai_gps(df[[col_y_raw]])
  }
  
  return(df)
}

library(tmaptools)

da <- converti_coordinate_decimali(dati_aggiustati)

#funzione per geotaggare i posti (già sistemati) nella colonna "luogo", creando anche una cache per fare in modo di non cercare mille volte lo stesso luogo.

# geotagga_luoghi_con_cache <- function(df, col_luogo = "luogo", col_lat = "coord_x", col_lon = "coord_y", cache_file = "./dati/cache_geocoding.csv") {
#   library(tmaptools)
#   library(dplyr)
#   
#   # Crea la cartella "dati" se non esiste
#   dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
#   
#   # Carica cache esistente o inizializza
#   if (file.exists(cache_file)) {
#     cache <- read.csv(cache_file, stringsAsFactors = FALSE)
#   } else {
#     cache <- data.frame(
#       luogo = character(),
#       lat = numeric(),
#       lon = numeric(),
#       stringsAsFactors = FALSE
#     )
#   }
#   
#   # Trova luoghi da geotaggare (non già con coordinate e non vuoti)
#   luoghi_da_geotaggare <- df %>%
#     filter(is.na(.data[[col_lat]]) & !is.na(.data[[col_luogo]]) & .data[[col_luogo]] != "") %>%
#     distinct(.data[[col_luogo]]) %>%
#     pull()
#   
#   luoghi_nuovi <- setdiff(luoghi_da_geotaggare, cache$luogo)
#   
#   # Geocodifica solo quelli non in cache
#   nuovi <- data.frame(luogo = luoghi_nuovi, lat = NA_real_, lon = NA_real_, stringsAsFactors = FALSE)
#   
#   for (i in seq_along(luoghi_nuovi)) {
#     luogo_corrente <- luoghi_nuovi[i]
#     res <- tryCatch(
#       geocode_OSM(luogo_corrente, as.data.frame = TRUE),
#       error = function(e) NULL
#     )
#     
#     if (!is.null(res)) {
#       nuovi$lat[i] <- res$lat
#       nuovi$lon[i] <- res$lon
#     }
#     
#     Sys.sleep(1)  # Rispetta i limiti di Nominatim
#     cat("Geocodificato:", i, "di", length(luoghi_nuovi), "-", luogo_corrente, "\n")
#   }
#   
#   # Unisce alla cache esistente e salva
#   cache <- bind_rows(cache, nuovi) %>%
#     distinct(luogo, .keep_all = TRUE)
#   
#   write.csv(cache, cache_file, row.names = FALSE)
#   
#   # Aggiunge le coordinate al dataframe originale
#   df <- df %>%
#     left_join(cache, by = setNames("luogo", col_luogo)) %>%
#     mutate(
#       !!col_lat := if_else(is.na(.data[[col_lat]]), lat, .data[[col_lat]]),
#       !!col_lon := if_else(is.na(.data[[col_lon]]), lon, .data[[col_lon]])
#     ) %>%
#     select(-lat, -lon)
#   
#   return(df)
# }

#questa prima funzione di geotag ha un problema di cache nel momento in cui non ha luoghi nuovi, ma sono tutti già nella cache.
#l'ho aggiornata per superare il problema

geotagga_luoghi_con_cache_v2 <- function(df, col_luogo = "luogo", col_lat = "coord_x", col_lon = "coord_y", cache_file = "./dati/cache_geocoding.csv") {
  library(tmaptools)
  library(dplyr)
  
  # Crea la cartella "dati" se non esiste
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  
  # Carica cache esistente o inizializza
  if (file.exists(cache_file)) {
    cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  } else {
    cache <- data.frame(
      luogo = character(),
      lat = numeric(),
      lon = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # Trova luoghi da geotaggare (non già con coordinate e non vuoti)
  luoghi_da_geotaggare <- df %>%
    filter(is.na(.data[[col_lat]]) & !is.na(.data[[col_luogo]]) & .data[[col_luogo]] != "") %>%
    distinct(.data[[col_luogo]]) %>%
    pull()
  
  luoghi_nuovi <- setdiff(luoghi_da_geotaggare, cache$luogo)
  
  if (length(luoghi_nuovi) > 0) {
    # Geocodifica solo quelli non in cache
    nuovi <- data.frame(luogo = luoghi_nuovi, lat = NA_real_, lon = NA_real_, stringsAsFactors = FALSE)
    
    for (i in seq_along(luoghi_nuovi)) {
      luogo_corrente <- luoghi_nuovi[i]
      res <- tryCatch(
        geocode_OSM(luogo_corrente, as.data.frame = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(res)) {
        nuovi$lat[i] <- res$lat
        nuovi$lon[i] <- res$lon
      }
      
      Sys.sleep(1)  # Rispetta i limiti di Nominatim
      cat("Geocodificato:", i, "di", length(luoghi_nuovi), "-", luogo_corrente, "\n")
    }
    
    # Unisce alla cache esistente e salva
    cache <- bind_rows(cache, nuovi) %>%
      distinct(luogo, .keep_all = TRUE)
    
    write.csv(cache, cache_file, row.names = FALSE)
  }
  
  # Aggiunge le coordinate al dataframe originale
  df <- df %>%
    left_join(cache, by = setNames("luogo", col_luogo)) %>%
    mutate(
      !!col_lat := if_else(is.na(.data[[col_lat]]), lat, .data[[col_lat]]),
      !!col_lon := if_else(is.na(.data[[col_lon]]), lon, .data[[col_lon]])
    ) %>%
    select(-lat, -lon)
  
  return(df)
}



#dopo un po' di prove, ho capito che OSM ha problemi con gli accenti, quindi cerco di ripulire per rimuovere le lettere accentate

da <- da %>% mutate(luogo = iconv(luogo, from = "UTF-8", to = "ASCII//TRANSLIT"))

da2 <- da %>% geotagga_luoghi_con_cache_v2()
view(da2)

#ora cerco di pulire la colonna degli indirizzi, per poi usarli per il geotag futuro

da2 <- da2 %>%
  mutate(indirizzo_prelievo = iconv(indirizzo_prelievo, from = "UTF-8", to = "ASCII//TRANSLIT"))

da2 <- da2 %>%
  mutate(indirizzo_prelievo = str_remove_all(indirizzo_prelievo, regex("\\bloc\\.?\\s*", ignore_case = TRUE)))

#voglio poi modificare la funzione precedente di geotag, facendo in modo che venga prima geotaggato il luogo già pulito, poi che venga geotaggato
#l'insieme di indirizzo, comune, provincia oppure solo comune e provincia se l'indirizzo non è riconosciuto

geotagga_completo_con_cache <- function(df,
                                        col_luogo = "luogo",
                                        col_lat = "coord_x",
                                        col_lon = "coord_y",
                                        col_indirizzo = "indirizzo_prelievo",
                                        col_comune = "comune",
                                        col_provincia = "provincia",
                                        cache_file = "./dati/cache_geocoding.csv") {
  library(tmaptools)
  library(dplyr)
  library(stringi)
  
  # Funzione di pulizia della stringa
  pulisci_stringa <- function(s) {
    s %>%
      stri_trans_general("Latin-ASCII") %>%     # rimuove accenti
      tolower() %>%                             # tutto minuscolo
      gsub("[^a-z0-9,'’ -]", "", .) %>%         # conserva lettere, numeri, virgole, spazi, apostrofi e trattini
      gsub("\\s+", " ", .) %>%                  # rimuove spazi multipli
      trimws()
  }
  
  
  # Crea cartella cache se serve
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  
  # Carica cache
  if (file.exists(cache_file)) {
    cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  } else {
    cache <- data.frame(luogo = character(), lat = numeric(), lon = numeric(), stringsAsFactors = FALSE)
  }
  
  # Righe da geotaggare
  da_geotaggare <- df %>%
    filter(is.na(.data[[col_lat]]) & is.na(.data[[col_lon]])) %>%
    mutate(
      luogo_grezzo = case_when(
        !is.na(.data[[col_luogo]]) & .data[[col_luogo]] != "" ~ .data[[col_luogo]],
        !is.na(.data[[col_indirizzo]]) & .data[[col_indirizzo]] != "" ~
          paste(.data[[col_indirizzo]], .data[[col_comune]], .data[[col_provincia]], sep = ", "),
        !is.na(.data[[col_comune]]) ~ paste(.data[[col_comune]], .data[[col_provincia]], sep = ", "),
        TRUE ~ NA_character_
      ),
      luogo_finale = pulisci_stringa(luogo_grezzo)
    ) %>%
    filter(!is.na(luogo_finale) & luogo_finale != "")
  
  luoghi_unici <- unique(da_geotaggare$luogo_finale)
  luoghi_nuovi <- setdiff(luoghi_unici, cache$luogo)
  
  if (length(luoghi_nuovi) > 0) {
    nuovi <- data.frame(luogo = luoghi_nuovi, lat = NA_real_, lon = NA_real_, stringsAsFactors = FALSE)
    
    for (i in seq_along(luoghi_nuovi)) {
      luogo_corrente <- luoghi_nuovi[i]
      res <- tryCatch(
        geocode_OSM(luogo_corrente, as.data.frame = TRUE),
        error = function(e) NULL
      )
      
      if (is.null(res) && grepl(",", luogo_corrente)) {
        fallback <- sub(".*, (.*?, .*?)$", "\\1", luogo_corrente)
        res <- tryCatch(
          geocode_OSM(fallback, as.data.frame = TRUE),
          error = function(e) NULL
        )
      }
      
      if (!is.null(res)) {
        nuovi$lat[i] <- res$lat
        nuovi$lon[i] <- res$lon
      }
      
      Sys.sleep(1)
      cat("Geocodificato:", i, "di", length(luoghi_nuovi), "-", luogo_corrente, "\n")
    }
    
    cache <- bind_rows(cache, nuovi) %>% distinct(luogo, .keep_all = TRUE)
    write.csv(cache, cache_file, row.names = FALSE)
    
    # Salva solo se sono stati geocodificati nuovi luoghi
    luoghi_non_trovati <- nuovi %>% filter(is.na(lat) | is.na(lon))
    if (nrow(luoghi_non_trovati) > 0) {
      write.csv(luoghi_non_trovati, "./dati/luoghi_non_trovati.csv", row.names = FALSE)
      message("Salvati ", nrow(luoghi_non_trovati), " luoghi non trovati in ./dati/luoghi_non_trovati.csv")
    }
  }
  
  # Aggiunge coordinate al df
  df <- df %>%
    mutate(
      luogo_grezzo = case_when(
        !is.na(.data[[col_luogo]]) & .data[[col_luogo]] != "" ~ .data[[col_luogo]],
        !is.na(.data[[col_indirizzo]]) & .data[[col_indirizzo]] != "" ~
          paste(.data[[col_indirizzo]], .data[[col_comune]], .data[[col_provincia]], sep = ", "),
        !is.na(.data[[col_comune]]) ~ paste(.data[[col_comune]], .data[[col_provincia]], sep = ", "),
        TRUE ~ NA_character_
      ),
      luogo_finale = pulisci_stringa(luogo_grezzo)
    ) %>%
    left_join(cache, by = c("luogo_finale" = "luogo")) %>%
    mutate(
      !!col_lat := if_else(is.na(.data[[col_lat]]), lat, .data[[col_lat]]),
      !!col_lon := if_else(is.na(.data[[col_lon]]), lon, .data[[col_lon]])
    ) %>%
    select(-luogo_grezzo, -luogo_finale, -lat, -lon)
  
  return(df)
}



#siccome il geotag funziona meglio se le provincie sono scritte per esteso, aggiusto questo fatto

# Mappa delle sigle provincia ai nomi estesi
mappa_province <- c(
  "BO" = "Bologna",
  "FE" = "Ferrara",
  "FC" = "Forlì-Cesena",
  "MO" = "Modena",
  "PR" = "Parma",
  "PC" = "Piacenza",
  "RA" = "Ravenna",
  "RE" = "Reggio Emilia",
  "RN" = "Rimini"
)

# Funzione per sostituire le sigle nel df
uniforma_province <- function(df, col_provincia = "provincia") {
  df[[col_provincia]] <- ifelse(
    df[[col_provincia]] %in% names(mappa_province),
    mappa_province[df[[col_provincia]]],
    df[[col_provincia]]
  )
  return(df)
}

da2 <- uniforma_province(da2, "provincia")

#siccome avevo già fatto una cache di luoghi in cui la provincia era in sigla, la aggiorno per non dover ritaggare tutti i luoghi
# Applica anche alla cache (se già esistente)
cache_file <- "./dati/cache_geocoding.csv"
if (file.exists(cache_file)) {
  cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  for (sigla in names(mappa_province)) {
    nome <- mappa_province[[sigla]]
    cache$luogo <- gsub(
      paste0(",\\s*", tolower(sigla), "$"),
      paste0(", ", tolower(nome)),
      cache$luogo
    )
  }
  # Riscrivi la cache aggiornata
  write.csv(cache, cache_file, row.names = FALSE)
}

da3 <- geotagga_completo_con_cache(da2)

view(da3)

#restano 4 luoghi geottaggabili in cui il geotag è fallito, cerco di aggiustarli

da3 <- da3 %>% 
  mutate(comune = replace(comune, comune %in% "Lesignano DèBagni", "Lesignano De' Bagni"),
         comune = replace(comune, comune %in% "S.Benedetto Val di Sambro", "San Benedetto Val di Sambro")) %>%
  mutate(
    coord_x = case_when(
      is.na(coord_x) & comune == "Bellaria-Igea Marina" ~ 44.1167,  # latitudine
      is.na(coord_x) & comune == "Sant'Arcangelo di Romagna" ~ 44.0678,
      TRUE ~ coord_x
    ),
    coord_y = case_when(
      is.na(coord_y) & comune == "Bellaria-Igea Marina" ~ 12.4833,  # longitudine
      is.na(coord_y) & comune == "Sant'Arcangelo di Romagna" ~ 12.4508,
      TRUE ~ coord_y
    )
  )

da4 <- geotagga_completo_con_cache(da3)
view(da4)

write.xlsx(da4, "./exports/coordinate conferimenti PRC2021006.xlsx")

# Mapping -----------------------------------------------------------------

#inizio col tentare di creare una mappa con tmap di tutti i campioni di cui ho la posizione

library(tmap)
library(sf)


# 1. Carica confini delle province italiane
# (usa un file .shp locale oppure recupera da ISTAT o GADM)
province_italiane <- st_read("./dati/limits_IT_provinces.geojson")

# 2. Filtra solo Emilia-Romagna
province_er <- province_italiane %>%
  filter(reg_name == "Emilia-Romagna")

# 3. Crea sf con i tuoi dati
campioni_sf <- da4 %>%
  filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326)  # lon, lat

# 4. Mappa
tmap_mode("view")  # o "plot" se vuoi per stampa

tm_shape(province_er) +
  tm_polygons(border.col = "gray60", alpha = 0.2) +
  tm_shape(campioni_sf) +
  tm_dots(col = "red", size = 0.25, alpha = 0.6, title = "Campioni") +
  tm_layout(title = "Campioni georeferenziati - Emilia-Romagna",
            legend.outside = TRUE)

#versione più interattiva

campioni_sf <- campioni_sf %>%
  mutate(
    lat = sf::st_coordinates(.)[,2],
    lon = sf::st_coordinates(.)[,1]
  )

tm_shape(province_er) +
  tm_borders() +
  tm_shape(campioni_sf) +
  tm_dots(
    fill = "red",
    size = 0.08,
    popup.vars = c("Latitudine" = "lat", "Longitudine" = "lon", "ID" = c("conferimento","anno_reg", "specie" = "specie"))
  )


#già questa va molto bene per i campioni totali. Prossimo step: dovrei integrare i dati di positività in sierologia ed in PCR Pancov
#per poter mappare i positivi. Inoltre, sui positivi, sarebbe bello discriminare graficamente le specie

