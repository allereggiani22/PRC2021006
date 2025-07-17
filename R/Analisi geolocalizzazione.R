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
         coord_y_raw = replace(coord_y_raw, coord_y_raw %in% "Y 11°24'46''", "11°54'46''"),
         coord_x_raw = replace(coord_x_raw, coord_x_raw %in% "X 44°59'17''", "X 43°59'17''"))
                                 

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




# Integrazione dati sierologia --------------------------------------------

sieri2022 <-  read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2022", col_types = c("skip", 
                                                             "text", "skip", "text", "text", "skip", 
                                                             "text", "numeric", "skip", "text", 
                                                             "text", "numeric", "text"))

sieri2023 <-  read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2023", col_types = c("text", 
                                                             "text", "text", "text", "text", "skip", 
                                                             "text", "text", "numeric", "text", 
                                                             "skip"))

sieri2024 <- read_excel("dati/11122024 Elenco Campioni PRC 2021_006.xlsx", 
                        sheet = "SIERI 2024", col_types = c("text", 
                                                            "text", "text", "text", "numeric", 
                                                            "skip", "text", "text", "numeric", 
                                                            "text", "skip"))


sieri2022 <- add_column(sieri2022,Anno= 2022, .after="Materiale")
sieri2023 <- add_column(sieri2023,Anno= 2023, .after="Materiale")
sieri2024 <- add_column(sieri2024,Anno= 2024, .after="Materiale")

sieri2022 <- sieri2022 %>% select(-6)
sieri2023 <- sieri2023 %>% select(-6)
sieri2024 <- sieri2024 %>% select(-6)


sieri <- bind_rows(sieri2022, sieri2023, sieri2024) #non ho rimosso dall'excel le S e le U dai nconf...

sieri <- clean_names(sieri)

#devo rimuovere le U e le S dagli nconf, quindi
sieri <- sieri %>%
  mutate(conf_orig = str_remove(conf_orig, " [SU]$"))

view(sieri)

sieri <- sieri %>% separate(.,conf_orig,c("conferimento","n_campione"), sep = "/") %>%
  mutate(n_campione = na.fill(n_campione, (1))) %>% 
  dplyr::rename(anno_reg = anno) %>% 
  mutate(conferimento = as.numeric(conferimento))

da4 <- read.xlsx("./exports/coordinate conferimenti PRC2021006.xlsx")


#mi sono sbagliato, non ho collassato i dati di matrici multiple in sieri_testati_n
#sieri_testati_n


sieri_clean <- sieri %>% 
  mutate(elisa = replace(elisa, elisa %in% c("INSUFF", "ASSENTE"), NA),
         specie = recode (specie, "SCOTTAIOLO" = "SCOIATTOLO")) %>% 
  filter(!is.na(elisa)) %>% 
  filter(!specie %in% c("GATTO")) %>% #ho rimosso il filtro sulla specie cinghiale, perché uno è stato testato. sopra non funzionava e lo contava, quindi non tornavano i numeri
  filter(!(conferimento == 32419 & elisa == "NEG")) %>% # il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. 
  #distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  filter(!(conferimento == 32422 & s_elisa == "NEG")) %>% # il conferimento 32422 è stato fatto in due aliquote, 
  #di cui solo una positiva per s_elisa
  mutate(
    elisa = ifelse(elisa == "POS", 1, 0),
    s_elisa = ifelse(s_elisa == "POS", 1, ifelse(s_elisa == "NEG", 0, NA)),
    materiale = ifelse(materiale %in% c("UMOR VITREO", "UMOR"), "UMOR ACQUEO", materiale),
    materiale = ifelse(materiale %in% c("UMOR ACQUEO", "MUSCOLO"), materiale, "SIERO"),
    specie = recode(specie,
                    "CAPRIOLO" = "ROE DEER",
                    "DAINO" = "FALLOW DEER",
                    "TASSO" = "BADGER",
                    "ISTRICE" = "PORCUPINE",
                    "LEPRE" = "HARE",
                    "RICCIO" = "HEDGEHOG",
                    "SCOIATTOLO" = "SQUIRREL",
                    "LUPO" = "WOLF",
                    "CERVO" = "DEER",
                    "FAINA" = "BEECH MARTEN",
                    "DELFINO" = "TURSIOPS",
                    "VOLPE" = "RED FOX",
                    "SILVILAGO" = "MARSH RABBIT",
                    "PUZZOLA" = "SKUNK",
                    "CONIGLIO" = "RABBIT",
                    "LONTRA" = "OTTER",
                    "SURICATO" = "MEERKAT",
                    "AGUTI DI AZARA" = "AZARA'S AGOUTI",
                    "RATTO" = "RAT",
                    "TOPO" = "MOUSE",
                    "GHIRO" = "DORMOUSE",
                    "GATTO SELVATICO" = "EUROPEAN WILDCAT",
                    "CANGURO" = "WALLABY",
                    "CINGHIALE" = "WILD BOAR",
                    "MARTORA" = "PINE MARTEN",
                    "SCIACALLO" = "JACKAL",
                    "FURETTO" = "FERRET"
                    )
  ) 


 #fino qui funziona tutto 
  #distinct(conferimento, .keep_all = TRUE) %>% QUESTO COMANDO MI ROVINA TUTTO SE PRIMA NON FACCIO IL PIVOT


  # group_by(specie) %>% 
  # summarise(
  #   Totale = n(),
  #   elisa = sum(elisa == "Pos"),
  #   s_elisa = sum(s_elisa == "Pos"),
  #   .groups = "drop"
  # ) %>% arrange(desc(elisa)) %>% janitor::adorn_totals() %>%  view() #non tornano i numeri... bisogna ragionare con più calma

# sieri_n_pos <- sieri %>% filter(elisa == "POS") %>%
#   filter(!(conferimento == 32422 & s_elisa == "NEG")) %>% 
#     filter(specie != "GATTO")


# Aggiunta successiva per verifica su dataset completo --------------------


#ATTENZIONE: CON IL FILTRO SUI POSITIVI FATTO QUI SOPRA, NON IMPORTO I DATI DI QUALI SONO NEGATIVI, NON POTENDO POI DISCRIMINARE QUELLI NON TESTATI... SISTEMO SOTTO

metadata <- sieri_clean %>%
  select(conferimento, n_campione, specie, anno_reg, provenienza, note, percent_pos) %>%
  distinct(conferimento, n_campione, .keep_all = TRUE)

pivot_data <- sieri_clean %>% group_by(conferimento, n_campione, materiale) %>%
  summarise(
    elisa = sum(elisa),
    s_elisa = sum(s_elisa, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(names_from = materiale, values_from = c(elisa,s_elisa), values_fill = 0)

sieri_clean <- left_join(metadata, pivot_data, by= c("conferimento", "n_campione"))

view(sieri_clean)

sieri_clean <- sieri_clean %>% 
  select(-percent_pos) %>%
  group_by(conferimento, n_campione, specie) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  rowwise() %>% 
  mutate(
    elisa = ifelse(sum(c_across(c(5:7))) >= 1, "Pos", "Neg"),
    s_elisa = ifelse(sum(c_across(c(8:10))) >= 1, "Pos", "Neg")
  ) %>% 
  select(-elisa_SIERO, -`elisa_UMOR ACQUEO`, -elisa_MUSCOLO, -s_elisa_SIERO, -`s_elisa_UMOR ACQUEO`, -s_elisa_MUSCOLO ) %>%
  relocate(elisa, .before = s_elisa)


#### fine aggiunta ###

#cerco di capire perché alcune righe dai sieri pos non ci sono in da4

righe_non_matchate <- sieri_n_pos %>%
  anti_join(da4, by = c("conferimento", "n_campione", "specie", "anno_reg"))

view(righe_non_matchate)
view(da4)


#right_join(da4, by= c("conferimento", "n_campione", "specie", "anno_reg")) %>% filter(elisa == "POS") %>% view()

write.xlsx(righe_non_matchate, file = "./exports/conferimenti da cercare.xlsx")

#trovate informazioni relative a conferimenti non matchati, aggiunte al file excel

righe_non_matchate <- read.xlsx("./exports/conferimenti da cercare.xlsx")
view(righe_non_matchate)

col_comuni <- intersect(names(da4), names(righe_non_matchate))

da4_sieri <- bind_rows(
  da4,
  righe_non_matchate %>% select(all_of(col_comuni))
)

view(da4_sieri)

da4_sieri <- geotagga_completo_con_cache(da4_sieri)
view(da4_sieri)

#ora posso integrare i dati di positività sierologica 

#da4_sieri ha le specie in italiano, gli do il recode prima

da4_sieri <- da4_sieri %>% mutate(specie = recode (specie,
                                       "CAPRIOLO" = "ROE DEER",
                                       "DAINO" = "FALLOW DEER",
                                       "TASSO" = "BADGER",
                                       "ISTRICE" = "PORCUPINE",
                                       "LEPRE" = "HARE",
                                       "RICCIO" = "HEDGEHOG",
                                       "SCOIATTOLO" = "SQUIRREL",
                                       "LUPO" = "WOLF",
                                       "CERVO" = "DEER",
                                       "FAINA" = "BEECH MARTEN",
                                       "DELFINO" = "TURSIOPS",
                                       "VOLPE" = "RED FOX",
                                       "SILVILAGO" = "MARSH RABBIT",
                                       "PUZZOLA" = "SKUNK",
                                       "CONIGLIO" = "RABBIT",
                                       "LONTRA" = "OTTER",
                                       "SURICATO" = "MEERKAT",
                                       "AGUTI DI AZARA" = "AZARA'S AGOUTI",
                                       "RATTO" = "RAT",
                                       "TOPO" = "MOUSE",
                                       "GHIRO" = "DORMOUSE",
                                       "GATTO SELVATICO" = "EUROPEAN WILDCAT",
                                       "CANGURO" = "WALLABY",
                                       "CINGHIALE" = "WILD BOAR",
                                       "MARTORA" = "PINE MARTEN",
                                       "SCIACALLO" = "JACKAL",
                                       "FURETTO" = "FERRET",
                                       "DONNOLA" = "WEASEL",
                                       "PIPISTRELLO" = "BAT",
                                       "PROCIONE" = "RACCOON",
                                       "TALPA" = "MOLE",
                                       "CINCILLA'" = "CHINCHILLA"
))

#devo risolvere degli intoppi --> il problema è che ci sono 1450 campioni di siero analizzati non presenti nel dataset scaricato da bobj

conf_test_mancanti <- anti_join(sieri_clean, da4_sieri, by = c("conferimento", "n_campione", "specie", "anno_reg"))

anti_join(sieri_clean, da4_sieri, by = c("conferimento", "n_campione", "specie", "anno_reg")) %>%
  distinct(specie) %>% arrange(specie)


#forzo l'inserzione delle righe in più? no sistemo poi aggiusto



#visto che ci sono 450 campioni in più, devo integrarli nel dataset. aggiungo sotto una sezione per farlo



# Estrazione dati per conferimenti solo siero -----------------------------

selvatici  <- read_excel("dati/Selvatici ER filone.xlsx", 
                            col_types = c("skip", "skip", "numeric", 
                                                   "date", "numeric", "numeric", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text"))


selvatici <- clean_names(selvatici)

view(selvatici)

view(conf_test_mancanti)

interessanti <- selvatici %>% filter(conferimento %in% conf_test_mancanti$conferimento) %>% distinct()

unique(interessanti$specie)

interessanti <- interessanti %>% filter(! specie %in% c("MERLO","RONDONE","GHIANDAIA","TORTORA","FRINGUELLO COMUNE","UPUPA", "PICCHIO VERDE", "CORNACCHIA GRIGIA"))

info_estratte <- estrai_info_note(interessanti)

view(info_estratte)

info_estratte %>% write.xlsx(., file = "./dati/conf mancanti info estratte.xlsx")

mancanti_corretti <- read_excel("./dati/mancanti_corretti.xlsx")

mancanti_corretti <- converti_coordinate_decimali(mancanti_corretti)

mancanti_corretti <- geotagga_completo_con_cache(mancanti_corretti)

view(mancanti_corretti)
mancanti_corretti %>% filter(is.na(coord_x)) %>% view()
view(conf_test_mancanti)

#capiamo cosa ancora non torna
da_integrare <- anti_join(conf_test_mancanti, mancanti_corretti, by=("conferimento")) %>% 
  mutate(conferimento = case_when(
    conferimento == 61453 ~ 61435,
    conferimento == 74663 ~ 74633,
    conferimento == 88429 ~ 88249,
    conferimento == 164532 ~ 164632,
    conferimento == 169329 ~ 179329,
    conferimento == 264957 ~ 261957,
    conferimento == 279623 ~ 279263,
    TRUE ~ conferimento),
    specie = case_when(
      conferimento == 74633 ~ "RED FOX",
      TRUE ~ specie)) %>% filter(! specie %in% c("WOLF", "WILD BOAR"))

view(da_integrare)



#correggo quelli che erano scritti male
conf_test_mancanti_2 <- conf_test_mancanti %>% 
 mutate(conferimento = case_when(
   conferimento == 61453 ~ 61435,
   conferimento == 74663 ~ 74633,
   conferimento == 88429 ~ 88249,
   conferimento == 164532 ~ 164632,
   conferimento == 169329 ~ 179329,
   conferimento == 264957 ~ 261957,
   conferimento == 279623 ~ 279263,
   TRUE ~ conferimento),
        specie = case_when(
          conferimento == 74633 ~ "RED FOX",
          TRUE ~ specie))

view(conf_test_mancanti_2)

da_integrare_dati <-
  selvatici %>% filter(conferimento %in% da_integrare$conferimento) %>% distinct()

da_integrare_dati <- estrai_info_note(da_integrare_dati)

view(da_integrare_dati)

da_integrare_dati <- da_integrare_dati %>% 
  mutate(coord_x_raw = case_when(
    conferimento == 74633 ~ "44°21'08''",
    TRUE ~ coord_x_raw),
    indirizzo_prelievo = case_when(
      indirizzo_prelievo == "LOC. DUE BANDIERE" ~ "due bandiere",
      TRUE ~ indirizzo_prelievo))

da_integrare_dati <- converti_coordinate_decimali(da_integrare_dati)

da_integrare_dati <- geotagga_completo_con_cache(da_integrare_dati)
 
mancanti_corretti2 <- bind_rows(mancanti_corretti, da_integrare_dati)

mancanti_corretti2 <- 
  mancanti_corretti2 %>% filter(conferimento != 189572) %>% filter (!(conferimento == 262264 & specie == "PIPISTRELLO DI SAVI"))

view(mancanti_corretti2)

rimanenti <- anti_join(conf_test_mancanti_2, mancanti_corretti2, by=("conferimento")) %>% view()
#restano fuori solo i lupi della lombradia, un cinghiale ed un furetto

view(da4_sieri)

integrati <- conf_test_mancanti_2 %>% group_by(conferimento, n_campione, anno_reg) %>% summarise() %>% 
  left_join(mancanti_corretti2, by= c("conferimento", "anno_reg")) %>% view()

anti_join(integrati, conf_test_mancanti_2)

da_correggere <- conf_test_mancanti_2 %>%
  group_by(conferimento, n_campione, anno_reg) %>%
  summarise(.groups = "drop") %>%
  left_join(mancanti_corretti2, by = "conferimento") %>%
  filter(!is.na(anno_reg.x), !is.na(anno_reg.y), anno_reg.x != anno_reg.y) %>%
  select(conferimento, n_campione)

conf_test_mancanti_2 <-  conf_test_mancanti_2 %>%
  left_join(da_correggere %>% mutate(da_correggere = TRUE),
            by = c("conferimento", "n_campione")) %>%
  mutate(anno_reg = case_when(
    da_correggere == TRUE & anno_reg == 2023 ~ 2022,
    TRUE ~ anno_reg
  )) %>%
  select(-da_correggere)

#ridò il comando di prima ora

integrati <- conf_test_mancanti_2 %>% group_by(conferimento, n_campione, anno_reg) %>% summarise() %>% 
  left_join(mancanti_corretti2, by= c("conferimento", "anno_reg")) %>% select(- n_campioni) %>%  view()

integrati <-  integrati %>% filter(! is.na(data_reg))
view(integrati)

#ora devo aggiungere integrati a da4_sieri, per poi unire le colonne degli elisa

cols_comuni <- intersect(names(da4_sieri), names(integrati))

integrati_al <- integrati %>% 
  mutate(provincia = recode(provincia,   
                                        "BO" = "Bologna",
                                        "FE" = "Ferrara",
                                        "FC" = "Forlì-Cesena",
                                        "MO" = "Modena",
                                        "PR" = "Parma",
                                        "PC" = "Piacenza",
                                        "RA" = "Ravenna",
                                        "RE" = "Reggio Emilia",
                                        "RN" = "Rimini"),
         specie = recode (specie,
                                 "CAPRIOLO" = "ROE DEER",
                                 "DAINO" = "FALLOW DEER",
                                 "TASSO" = "BADGER",
                                 "ISTRICE" = "PORCUPINE",
                                 "LEPRE" = "HARE",
                                 "RICCIO" = "HEDGEHOG",
                                 "SCOIATTOLO" = "SQUIRREL",
                                 "LUPO" = "WOLF",
                                 "CERVO" = "DEER",
                                 "FAINA" = "BEECH MARTEN",
                                 "DELFINO" = "TURSIOPS",
                                 "VOLPE" = "RED FOX",
                                 "SILVILAGO" = "MARSH RABBIT",
                                 "PUZZOLA" = "SKUNK",
                                 "CONIGLIO" = "RABBIT",
                                 "LONTRA" = "OTTER",
                                 "SURICATO" = "MEERKAT",
                                 "AGUTI DI AZARA" = "AZARA'S AGOUTI",
                                 "RATTO" = "RAT",
                                 "TOPO" = "MOUSE",
                                 "GHIRO" = "DORMOUSE",
                                 "GATTO SELVATICO" = "EUROPEAN WILDCAT",
                                 "CANGURO" = "WALLABY",
                                 "CINGHIALE" = "WILD BOAR",
                                 "MARTORA" = "PINE MARTEN",
                                 "SCIACALLO" = "JACKAL",
                                 "FURETTO" = "FERRET",
                                 "DONNOLA" = "WEASEL",
                                 "PIPISTRELLO" = "BAT",
                                 "PROCIONE" = "RACCOON",
                                 "TALPA" = "MOLE",
                                 "CINCILLA'" = "CHINCHILLA")) %>% 
  select(all_of(cols_comuni))

da4_sieri <- bind_rows(da4_sieri, integrati_al)

#prima di integrare dati sieri, devo correggere l'anno delle lepri

sieri_clean <- sieri_clean %>%  mutate(anno_reg = case_when(
  conferimento %in% da_correggere$conferimento & anno_reg == 2023 ~ 2022,
  TRUE ~ anno_reg))

sieri_clean <- sieri_clean %>% anti_join(rimanenti, by = c("conferimento", "n_campione", "anno_reg"))
sieri_clean <- sieri_clean %>%  mutate(conferimento = case_when(
  conferimento == 61453 & anno_reg == 2023 ~ 61435,
  conferimento == 74663 & anno_reg == 2024 ~ 74633,
  conferimento == 88429 & anno_reg == 2024 ~ 88249,
  conferimento == 164532 & anno_reg == 2023 ~ 164632,
  conferimento == 169329 & anno_reg == 2024 ~ 179329,
  conferimento == 264957 & anno_reg == 2024 ~ 261957,
  conferimento == 279623 & anno_reg == 2024 ~ 279263,
  TRUE ~ conferimento),
  specie = case_when(
    conferimento == 74633 ~ "RED FOX",
    conferimento == 148325 & anno_reg == 2023 ~ "PORCUPINE",
    conferimento == 227483 & anno_reg == 2023 ~ "BADGER",
    conferimento == 285481 & anno_reg == 2024 ~ "ROE DEER",
    conferimento == 255856 & anno_reg == 2024 ~ "WOLF",
    conferimento == 260514 & anno_reg == 2024 ~ "RED FOX",
    TRUE ~ specie))

#ho notato alcune righe duplicate in da4_sieri
set.seed(123)  # Per rendere la scelta riproducibile (opzionale)
da4_sieri_dedup <- da4_sieri %>%
  as_tibble() %>%
  group_by(conferimento, n_campione, anno_reg) %>%
  slice_sample(n = 1) %>%
  ungroup()

da4_sieri_dedup <- da4_sieri_dedup %>%
  mutate(specie = case_when(
    conferimento == 74633 & anno_reg == 2024 ~ "RED FOX",
    conferimento == 227483 & anno_reg == 2023 ~ "BADGER",
    conferimento == 227426 & anno_reg == 2024 ~ "JACKAL",
    conferimento == 241029 & anno_reg == 2023 ~ "RAT",
    TRUE ~ specie  # lascia invariato per gli altri
  ))


  
da4_sieri <- left_join(da4_sieri_dedup, sieri_clean, by = c("conferimento", "n_campione", "specie", "anno_reg"))

# anti_join(sieri_clean, p1, by=c("conferimento", "n_campione", "specie", "anno_reg")) %>% view()
# 
# anti_join(da4_sieri, sieri_clean, by = c("conferimento", "n_campione", "specie", "anno_reg")) %>% view()
# 
# anti_join(sieri_clean, da4_sieri, by = c("conferimento", "n_campione", "specie", "anno_reg")) %>% view()

# integrazione dati Pancov ------------------------------------------------

#cerco di unire i dati pancov, per ottenere il dataset completo

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

# Riempire celle vuote con valore precedente


dati <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) #più ordinato di versione precedente



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
         materiale = replace(materiale, materiale %in% "TR + PO", "TRACHEA + POLMONE"))


dt_pancov <- dt_an %>%
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         pancov = ifelse(pancov=="POS",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = pancov, values_fill = 0) %>% 
  select(-progr) %>% 
  select(-sacco, -piastra_estrazione, -data_esito,-note) %>%
  group_by(conf_orig, specie,provenienza, anno) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>%
  mutate(somma = sum(c_across(c(5:17))),
         pos_pancov = ifelse(somma >=1, "POS", "NEG")) %>% 
  select(-(5:17)) %>% select(- somma) %>% dplyr::rename(anno_reg = anno) %>% select(- provenienza) %>% 
  separate(.,conf_orig,c("conferimento","n_campione"), sep = "/") %>% 
  mutate(n_campione = na.fill(n_campione, (1))) %>% 
  mutate(conferimento = as.numeric(conferimento),
         specie = recode(specie,
          "CAPRIOLO" = "ROE DEER",
          "DAINO" = "FALLOW DEER",
          "TASSO" = "BADGER",
          "ISTRICE" = "PORCUPINE",
          "LEPRE" = "HARE",
          "RICCIO" = "HEDGEHOG",
          "SCOIATTOLO" = "SQUIRREL",
          "LUPO" = "WOLF",
          "CERVO" = "DEER",
          "FAINA" = "BEECH MARTEN",
          "DELFINO" = "TURSIOPS",
          "VOLPE" = "RED FOX",
          "SILVILAGO" = "MARSH RABBIT",
          "PUZZOLA" = "SKUNK",
          "CONIGLIO" = "RABBIT",
          "LONTRA" = "OTTER",
          "SURICATO" = "MEERKAT",
          "AGUTI DI AZARA" = "AZARA'S AGOUTI",
          "RATTO" = "RAT",
          "TOPO" = "MOUSE",
          "GHIRO" = "DORMOUSE",
          "GATTO SELVATICO" = "EUROPEAN WILDCAT",
          "CANGURO" = "WALLABY",
          "CINGHIALE" = "WILD BOAR",
          "MARTORA" = "PINE MARTEN",
          "SCIACALLO" = "JACKAL",
          "FURETTO" = "FERRET",
          "PIPISTRELLO" = "BAT",
          "DONNOLA" = "WEASEL",
          "PROCIONE" = "RACCOON",
          "CINCILLA'" = "CHINCHILLA",
          "TALPA" = "MOLE"),
         specie = case_when(
           conferimento == 74633 & anno_reg == 2024 ~ "RED FOX",
           conferimento == 227483 & anno_reg == 2023 ~ "BADGER",
           TRUE ~ specie))


da_completo_2 <- da4_sieri %>% left_join(dt_pancov, by = c("conferimento", "n_campione", "specie", "anno_reg"))
da_completo_3 <- da4_sieri %>% left_join(dt_pancov, by = c("conferimento", "n_campione", "specie", "anno_reg"))

anti_join(dt_pancov,da_completo_3, by= c("conferimento", "n_campione", "specie", "anno_reg")) %>% view()


view(da_completo_3)

#problema: non ero riuscito a non inserire dei negativi falsi in s_elisa: li rimuovo forzatamente
da_completo_3 <- da_completo_3 %>%
  mutate(elisa = toupper(elisa),
         s_elisa = toupper(s_elisa)) %>% 
  mutate(s_elisa = case_when(
    elisa == "NEG" ~ NA,
    TRUE ~ s_elisa
  ))

da_completo_3 <- da_completo_3 %>% 
  mutate(coord_y = case_when(
    conferimento ==  262254 & anno_reg == 2024 ~ 12.36972222222,
    conferimento ==  310680 & anno_reg == 2024 ~ 12.54972,
    TRUE ~ coord_y
  ))

#write.xlsx(da_completo, file = "./dati/dataset PRC completo.xlsx")
write.xlsx(da_completo_3, file = "./dati/dataset PRC completo 17072025.xlsx")
# Mapping -----------------------------------------------------------------

da_completo <- read.xlsx("./dati/dataset PRC completo 17072025.xlsx")



#inizio col tentare di creare una mappa con tmap di tutti i campioni di cui ho la posizione

library(tmap)
library(sf)

#da4 <- read.xlsx("./exports/coordinate conferimenti PRC2021006.xlsx")

# 1. Carica confini delle province italiane
province_italiane <- st_read("./dati/limits_IT_provinces.geojson")

# 2. Filtra solo Emilia-Romagna
province_er <- province_italiane %>%
  filter(reg_name == "Emilia-Romagna")

#2. Alternativo
prov_campionate <- province_italiane %>% 
  filter(reg_name == "Emilia-Romagna" | prov_name == "Pavia")

# 3. Crea sf con i tuoi dati
campioni_sf <- da_completo %>% filter(provincia != "Pavia" | is.na(provincia)) %>% 
  filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326)  # lon, lat

#versione più interattiva - (va bene anche per view mode)

campioni_sf <- campioni_sf %>%
  mutate(
    lat = sf::st_coordinates(.)[,2],
    lon = sf::st_coordinates(.)[,1]
  )

# 4. Mappa


# Mappe statiche per pubblicazione ----------------------------------------



tmap_mode("plot")  # o "plot" se vuoi per stampa


map1_campionamenti <- tm_shape(province_er) +
  tm_polygons(col = "gray60", fill_alpha = 0.2) +
  tm_shape(campioni_sf) +
  tm_dots(fill = "red", size = 0.1, fill_alpha = 0.6, title = "Campioni") +
  tm_layout(frame = F)

tmap_save(map1_campionamenti, "./exports/Total sampling map.png", dpi = 600)

#versione per tmap - view mode

# tmap_mode("view")
# 
# map1_campionamenti <- tm_shape(province_er) +
#   tm_borders() +
#   tm_shape(campioni_sf) +
#   tm_dots(
#     fill = "red",
#     size = 0.08,
#     popup.vars = c("Latitudine" = "lat", "Longitudine" = "lon", "ID" = c("conferimento","anno_reg", "specie" = "specie"))
#   )+
#   tm_title("Campionamenti totali")


#già questa va molto bene per i campioni totali. Prossimo step: dovrei integrare i dati di positività in sierologia ed in PCR Pancov
#per poter mappare i positivi. Inoltre, sui positivi, sarebbe bello discriminare graficamente le specie

#mappa positivi sierologia

campioni_pos_sieri_sf <- da_completo %>% filter(s_elisa == "POS") %>% filter(provincia != "Pavia" | is.na(provincia)) %>% 
  filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326) %>%   # lon, lat
  mutate(
    lat = sf::st_coordinates(.)[,2],
    lon = sf::st_coordinates(.)[,1]
  )

palette_specie <- c(
  "ROE DEER" = "#1f77b4",
  "RED FOX" = "#ff7f0e",
  "WOLF" = "#2ca02c",
  "HEDGEHOG" = "#800080",
  "HARE" = "#db7093",
  "RAT" = "#ee82ee",
  "BADGER" = "red3",
  "PORCUPINE" = "#00ced1"
)

# map2_elisa_pos <- tm_shape(province_er) +
#   tm_polygons(col = "gray60", fill_alpha = 0.2) +
#   tm_shape(campioni_pos_sieri_sf) +
#   tm_squares(
#     fill = "specie",
#     size = 0.35,
#     popup.vars = c(
#       "Latitudine" = "lat",
#       "Longitudine" = "lon",
#       "Conferimento" = "conferimento",
#       "Anno" = "anno_reg",
#       "Specie" = "specie"
#     ),
#     fill.scale  = tm_scale_categorical(values = palette_specie)
#   )+
#   tm_title("Positivi ELISA RBD")

campioni_pos_pancov_sf <- da_completo %>% filter(pos_pancov == "POS") %>% filter(provincia != "Pavia" | is.na(provincia)) %>% 
  filter(!is.na(coord_x) & !is.na(coord_y)) %>%
  st_as_sf(coords = c("coord_y", "coord_x"), crs = 4326) %>%   # lon, lat
  mutate(
    lat = sf::st_coordinates(.)[,2],
    lon = sf::st_coordinates(.)[,1])



# map3_pos_pancov <- tm_shape(province_er) +
#   tm_polygons(col = "gray60", fill_alpha = 0.2) +
#   tm_shape(campioni_pos_pancov_sf) +
#   tm_dots(
#     fill = "specie",
#     size = 0.35,
#     popup.vars = c(
#       "Latitudine" = "lat",
#       "Longitudine" = "lon",
#       "Conferimento" = "conferimento",
#       "Anno" = "anno_reg",
#       "Specie" = "specie"
#     ),
#     fill.scale  = tm_scale_categorical(values = palette_specie)
#   )+
#   tm_title("Positivi Pancov")

map4_pos_totali <- tm_shape(province_er) +
  tm_polygons(col = "gray60", fill_alpha = 0.2) +
  tm_shape(campioni_pos_sieri_sf) +
  tm_squares(
    fill = "specie",
    size = 0.4,
    popup.vars = c(
      "Latitudine" = "lat",
      "Longitudine" = "lon",
      "Conferimento" = "conferimento",
      "Anno" = "anno_reg",
      "Specie" = "specie"
    ),
    fill.scale  = tm_scale_categorical(values = palette_specie),
    fill.legend = tm_legend("ELISA positive")
  )+
  
  tm_shape(campioni_pos_pancov_sf) +
  tm_dots(
    fill = "specie",
    size = 0.4,
    popup.vars = c(
      "Latitudine" = "lat",
      "Longitudine" = "lon",
      "Conferimento" = "conferimento",
      "Anno" = "anno_reg",
      "Specie" = "specie"
    ),
    fill.scale  = tm_scale_categorical(values = palette_specie),
    fill.legend = tm_legend("PanCoV positive")
  )+
  tm_layout(frame = F)

tmap_save(map4_pos_totali, "./exports/Total positive map.png", dpi = 600)

# Mappe interattive per prova/web -----------------------------------------



#provo a fare una mappa unica

campioni_pos_pancov_sf <- campioni_pos_pancov_sf %>%
  dplyr::mutate(specie_pancov = specie)

campioni_pos_sieri_sf <- campioni_pos_sieri_sf %>%
  dplyr::mutate(specie_siero = specie)

# Palette condivisa per tutte le specie (personalizzabile)
palette_specie <- c(
  "CAPRIOLO" = "#1f77b4",
  "VOLPE" = "#ff7f0e",
  "LUPO" = "#2ca02c",
  "RICCIO" = "#800080",
  "LEPRE" = "#db7093",
  "RATTO" = "#ee82ee",
  "TASSO" = "red3",
  "ISTRICE" = "#00ced1"
)



tm_shape(province_er)+
  tm_borders(group = "confini provinciali"
             )+
  
  
  tm_shape(campioni_pos_pancov_sf) +
  tm_squares(
    fill = "specie_pancov",
    size = 0.35,
    popup.vars = c(
      "Latitudine" = "lat",
      "Longitudine" = "lon",
      "Conferimento" = "conferimento",
      "Anno" = "anno_reg",
      "Specie" = "specie"
    ),
    fill.scale  = tm_scale_categorical(values = palette_specie),
    group = "Positivi Pancov",
    )+
  
  tm_shape(campioni_pos_sieri_sf) +
  tm_squares(
    fill = "specie_siero",
    size = 0.35,
    popup.vars = c(
      "Latitudine" = "lat",
      "Longitudine" = "lon",
      "Conferimento" = "conferimento",
      "Anno" = "anno_reg",
      "Specie" = "specie"
    ),
    fill.scale  = tm_scale_categorical(values = palette_specie),
    group = "Positivi ELISA RBD",
    options = opt_tm_squares()
  )+
  
  
  tm_shape(campioni_sf) +
  tm_dots(
    fill = "red",
    size = 0.1,
    popup.vars = c("Latitudine" = "lat", "Longitudine" = "lon", "ID" = c("conferimento","anno_reg", "specie" = "specie")),
    group = "Campionamenti"
  )+
  tm_title("Campioni PRC 2021 006")

#va ottimizzato, devo capire cosa fare comparire all'avvio....
# risposta: pre che non si possa fare con tmap. Provo ad utilizzare leaflet

library(leaflet)
library(dplyr)
library(sf)

# Definisci una palette coerente per tutte le specie
specie_livelli <- unique(c(
  #campioni_sf$specie,
  campioni_pos_sieri_sf$specie,
  campioni_pos_pancov_sf$specie
))

pal <- colorFactor(
  palette = palette_specie,
  domain = names(palette_specie)
)

campioni_count_pancov <- campioni_pos_pancov_sf %>%
  count(geometry, name = "n_punti")

campioni_count_sieri <- campioni_pos_sieri_sf %>%
  count(geometry, name = "n_punti")


campioni_sf_pancov_enriched <- st_join(campioni_pos_pancov_sf, campioni_count_pancov, join = st_equals)

campioni_sf_sieri_enriched <- st_join(campioni_pos_sieri_sf, campioni_count_sieri, join = st_equals)


# Crea la mappa
mappa_completa <- leaflet() %>%
  # Aggiungi base map
  addProviderTiles("CartoDB.Positron") %>%
  
  # Aggiungi confini provinciali
  addPolygons(data = province_er,
              color = "black",
              weight = 1,
              fillOpacity = 0) %>%
  
  # Aggiungi Campionamenti totali
  addCircleMarkers(
    data = campioni_sf,
    radius = 1.5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    label = ~paste0("Specie: ", specie),
    popup = ~paste("Conferimento:", conferimento, "<br>",
                   "Anno:", anno_reg, "<br>",
                   "Lat:", lat, "<br>",
                   "Lon:", lon),
    group = "Campionamenti"
  ) %>%
  
  # Aggiungi Positivi ELISA
  # addCircleMarkers(
  #   data = campioni_pos_sieri_sf,
  #   radius = 4,
  #   color = ~pal(specie),
  #   stroke = TRUE,
  #   weight = 1,
  #   fillOpacity = 1,
  #   label = ~paste0("ELISA - Specie: ", specie),
  #   popup = ~paste("Conferimento:", conferimento, "<br>",
  #                  "Anno:", anno_reg, "<br>",
  #                  "Lat:", lat, "<br>",
  #                  "Lon:", lon),
  #   group = "Positivi ELISA"
  # ) %>%
  
addCircleMarkers(
  data = campioni_sf_sieri_enriched,
  radius = ~2 + (0.5*n_punti),  # Aumenta il raggio se ci sono più punti sovrapposti
  color = ~pal(specie),
  stroke = TRUE,
  weight = 1,
  fillOpacity = 0.8,
  label = ~paste0("N campioni in questo punto: ", n_punti),
  popup = ~paste("Conferimento:", conferimento, "<br>",
                 "Anno:", anno_reg, "<br>",
                 "Lat:", lat, "<br>",
                 "Lon:", lon,"<br>",
                 "Specie: ", specie),
  group = "Positivi ELISA"
) %>% 


  # Aggiungi Positivi Pancov
  
  # addCircleMarkers(
  #   data = campioni_pos_pancov_sf,
  #   radius = 2,
  #   color = ~pal(specie),
  #   stroke = TRUE,
  #   weight = 1,
  #   fillOpacity = 1,
  #   label = ~paste0("Pancov - Specie: ", specie),
  #   popup = ~paste("Conferimento:", conferimento, "<br>",
  #                  "Anno:", anno_reg, "<br>",
  #                  "Lat:", lat, "<br>",
  #                  "Lon:", lon),
  #   group = "Positivi Pancov"
  #   ) %>%

addCircleMarkers(
  data = campioni_sf_pancov_enriched,
  radius = ~2 + (0.5*n_punti),  # Aumenta il raggio se ci sono più punti sovrapposti
  color = ~pal(specie),
  stroke = TRUE,
  weight = 1,
  fillOpacity = 0.8,
  label = ~paste0("N campioni in questo punto: ", n_punti),
  popup = ~paste("Conferimento:", conferimento, "<br>",
                 "Anno:", anno_reg, "<br>",
                 "Lat:", lat, "<br>",
                 "Lon:", lon, "<br>",
                 "Specie: ", specie),
  group = "Positivi Pancov"
) %>% 
  
  # Aggiungi legenda
 # addLegend("bottomright", pal = pal, values = specie_livelli,
  #          title = "Specie", opacity = 1) %>%
  
  # Legenda come layer separato
  addLegend("bottomright", pal = pal, values = specie_livelli,
            title = "Specie", opacity = 1,
            group = "Legenda specie") %>%
  
  # Aggiungi controllo layer
  addLayersControl(
    overlayGroups = c("Campionamenti", "Positivi ELISA", "Positivi Pancov", "Legenda specie"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Disattiva visibilità iniziale di alcuni gruppi
  hideGroup(c("Positivi ELISA", "Positivi Pancov"))

mappa_completa


#per risolvere problema di conferimenti che abbiano la stessa geolocalizzazione:

#1. usare clusterOptions = markerClusterOptions(maxClusterRadius = 0.1) come argomento di addCircleMarkers,
#ma secondo me raggruppa troppo. Opzione raggio a 0.1 per avere solo quelli in stesso punto clusterizzati

# . mettere numero come etichetta e dimensione maggiore del marker


# campioni_count <- campioni_pos_pancov_sf %>%
#   count(geometry, name = "n_punti")
# 
# 
# campioni_sf_enriched <- st_join(campioni_pos_pancov_sf, campioni_count, join = st_equals)

# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
  # addCircleMarkers(
  #   data = campioni_sf_enriched,
  #   radius = ~2 + (0.5*n_punti),  # Aumenta il raggio se ci sono più punti sovrapposti
  #   color = ~pal(specie),
  #   stroke = TRUE,
  #   weight = 1,
  #   fillOpacity = 0.8,
  #   label = ~paste0("N campioni in questo punto: ", n_punti),
  #   popup = ~paste("Conferimento:", conferimento, "<br>",
  #                  "Anno:", anno_reg, "<br>",
  #                  "Lat:", lat, "<br>",
  #                  "Lon:", lon)
  # )


# Analisi numeriche sul dataset completo ----------------------------------

da_completo <- read.xlsx("./dati/dataset PRC completo 17072025.xlsx")


#quanti animali campionati per specie?

da_completo %>% group_by(specie) %>% summarise(N_individui = n()) %>% arrange(desc(N_individui)) %>% janitor::adorn_totals() %>% view()

#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

da_completo %>%  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(pos_pancov == "POS", na.rm = T), NEG= sum(pos_pancov == "NEG", na.rm = T), 
            UNTESTED = sum(is.na(pos_pancov))) %>% 
  arrange(desc(N_individui)) %>% janitor::adorn_totals() %>% view() 

#summary positivi sierologia per specie

da_completo %>% group_by(specie) %>%  
  summarise(
    Totale = n(),
    elisa_pos = sum(elisa == "POS", na.rm = T),
    elisa_neg = sum(elisa == "NEG", na.rm = T),
    elisa_NT = sum(is.na(elisa)),
    s_elisa_pos = sum(s_elisa == "POS", na.rm = T),
    s_elisa_neg = sum(s_elisa == "NEG", na.rm = T),
    .groups = "drop") %>%
  arrange(desc(elisa_pos)) %>% janitor::adorn_totals() %>% view() #mancano diversi animali rispetto ad ECV..., perché? Perché non presenti nel dataset

    
