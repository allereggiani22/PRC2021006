source(here('R', 'librerie.R'))

library(Biostrings)


#COI sequences

# COI sequences -----------------------------------------------------------


fasta_file <- readDNAStringSet("dati/Allineamento per albero relazione finale TRIMMATO.fas")

#Rimuovere " FR" e simili da tutti gli header
new_headers <- names(fasta_file) %>% 
  gsub("( FR| R| F| F-R V)$", "",.) %>% 
  trimws() %>% 
  gsub("RICCIO", "HEDGEHOG",.) %>% 
  gsub("CAPRIOLO", "ROE DEER",.) %>% 
  gsub("VOLPE", "RED FOX",.) %>% 
  gsub("ISTRICE", "PORCUPINE",.) %>% 
  gsub("LEPRE", "HARE",.) %>% 
  gsub("LUPO", "WOLF",.) %>% 
  gsub("( INT| T RETT| TRACHEA| RETTO| FECI)$", "",.) %>% 
  ifelse(
    grepl("^CORONA HEDGEHOG", .),  # Controlla se l'header inizia già con "CORONA HEDGEHOG"
    .,                            # Lascia invariato l'header se contiene già "CORONA HEDGEHOG"
    gsub("(.*)(HEDGEHOG)(.*)",                    # Altrimenti, applica la modifica
         "CORONA HEDGEHOG \\1\\3",                # Sposta "HEDGEHOG" all'inizio con uno spazio aggiunto
         .)) %>% 
  ifelse(
    grepl("^CORONA ROE DEER", .),  
    .,                          
    gsub("(.*)(ROE DEER)(.*)",
         "CORONA ROE DEER \\1\\3",
         .)) %>% 
  ifelse(
    grepl("^CORONA RED FOX", .),  
    .,                          
    gsub("(.*)(RED FOX)(.*)",
         "CORONA RED FOX \\1\\3",
         .)) %>% 
  ifelse(
    grepl("^CORONA PORCUPINE", .),  
    .,                          
    gsub("(.*)(PORCUPINE)(.*)",
         "CORONA PORCUPINE \\1\\3",
         .)) %>% 
  ifelse(
    grepl("^CORONA HARE", .),  
    .,                          
    gsub("(.*)(HARE)(.*)",
         "CORONA HARE \\1\\3",
         .)) %>% 
  ifelse(
    grepl("^CORONA WOLF", .),  
    .,                          
    gsub("(.*)(WOLF)(.*)",
         "CORONA WOLF \\1\\3",
         .)) %>%
  trimws() %>%
  # Aggiungi " 2023" agli header tra 100 e 144, a meno che non lo abbiano già
  {.[100:144] <- ifelse(
    grepl(" 2023$", .[100:144]),  # Controlla se termina con " 2023"
    .[100:144],                   # Mantieni invariato
    paste0(.[100:144], " 2023")   # Aggiungi " 2023"
  ); .} %>%
  # Aggiungi " 2024" agli header tra 145 e 157, a meno che non lo abbiano già
  {.[145:157] <- ifelse(
    grepl(" 2024$", .[145:157]),  # Controlla se termina con " 2024"
    .[145:157],                   # Mantieni invariato
    paste0(.[145:157], " 2024")   # Aggiungi " 2023"
  ); .} %>%
  gsub(" mo ", " MO ", .) %>%
  gsub(" (INT FR|FR) ", " ",.)

  
 new_headers <-  new_headers %>% 
    {
      .[126] <- gsub("(^MO 4638)(.*)", "CORONA HEDGEHOG MO 4638 2023", .[126])  # modifico solo la riga 126, perché non era indicata la specie
      .
    }
names(fasta_file) <- new_headers
 
writeXStringSet(fasta_file, filepath = "exports/Allineamento modificato per EUSV 07.01.2025.fas")


# rinominare headers per submission genbank -------------------------------

source(here('R', 'librerie.R'))

library(Biostrings)


#Voglio rinominare le sequenze ottenute da noi ed usate in albero dell'articolo per sottometterle in genbank
#assegnando numerazione progressiva ma salvandomi una tabella con la corrispondenza



fasta_file <- readDNAStringSet("dati/Allineamento per genbank - v1.fas")

original_names <- names(readDNAStringSet("dati/Allineamento per genbank - v1.fas"))


new_headers <- paste0("Seq", seq_along(names(fasta_file)), "_", names(fasta_file))
short_headers <- paste0("Seq", seq_along(names(fasta_file)))
# Assegnare i nuovi header al DNAStringSet
names(fasta_file) <- new_headers

# Salvare il file FASTA modificato
writeXStringSet(fasta_file, "exports/Seq per genbank nomi completi.fas")

#nomi con solo progressivi
names(fasta_file) <- short_headers
writeXStringSet(fasta_file, "exports/seq per genbank nomi corti.fas")

write(names(fasta_file), "exports/new headers.txt")

# ora voglio fare la tabella di associazione con anche dati rilevanti presi dagli headers

library(stringr)
library(dplyr)
library(tibble)

# original_names deve essere un vettore di caratteri
# contenente i nomi originali delle sequenze FASTA
# es: original_names <- names(readDNAStringSet("file.fas"))

parsed <- lapply(original_names, function(x) {
  
  # 1. Spezza l'header in singole parole
  w <- str_split(x, "\\s+")[[1]]
  
  # 2. L'anno è sempre l'ultima parola
  anno <- tail(w, 1)
  
  # 3. Rimuove l'anno per lavorare sul resto
  core <- w[-length(w)]
  
  # 4. Identifica il token che rappresenta il campione numerico
  #    (numero semplice o numero-numero)
  num_idx <- which(str_detect(core, "^\\d+$|^\\d+-\\d+$"))
  
  # 5. Caso di sicurezza: se non troviamo alcun campione valido
  if (length(num_idx) == 0) {
    return(list(
      specie   = NA_character_,
      campione = NA_character_,
      anno     = anno
    ))
  }
  
  # 6. Prendiamo l'ultima occorrenza numerica
  #    (per sicurezza se ce ne fosse più di una)
  camp_start <- max(num_idx)
  
  # 7. Se subito prima del numero c'è "MO",
  #    includiamolo come parte del campione
  if (camp_start > 1 && core[camp_start - 1] == "MO") {
    camp_start <- camp_start - 1
  }
  
  # 8. Costruzione delle variabili finali
  campione <- paste(core[camp_start:length(core)], collapse = " ")
  specie   <- paste(core[2:(camp_start - 1)], collapse = " ")
  
  # 9. Ritorna una lista che diventerà una riga della tabella
  list(
    specie   = specie,
    campione = campione,
    anno     = anno
  )
})

# 10. Costruzione della tabella finale
df <- tibble(
  seq_id        = paste0("Seq", seq_along(original_names)),
  original_name = original_names
) %>%
  bind_cols(bind_rows(parsed))

# 11. Controllo minimo di coerenza:
#     se qui fallisce, gli header non rispettano le regole attese
stopifnot(all(!is.na(df$anno)))

# 12. Salvataggio dei metadata su file
write.csv(df, "exports/metadata_sequenze.csv", row.names = FALSE)
write.xlsx(df, "exports/metadata_sequenze.xlsx")

