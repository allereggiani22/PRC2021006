source(here('R', 'librerie.R'))



# dt2022 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
#                    col_types = c("numeric", "text", "text", 
#                                  "numeric", "text", "text", "date", 
#                                  "text", "text", "date", "text", "date", 
#                                  "text", "text", "text", "skip"))
# 
# dt2023 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
#                                                     sheet = "Campioni PRC 2023", col_types = c("numeric", 
#                                                                                                "text", "text", "numeric", "text", 
#                                                                                                "text", "text", "text", "date", "text", 
#                                                                                               "date", "text", "text", "text", "skip"))

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
# conferimenti <- unique(dati$`conf_orig`)
# table(conferimenti)

# Riempire celle vuote con valore precedente ------------------------------


# dati_2022 <- dati %>% 
#   mutate(conf_orig = na.locf(conf_orig),
#          provenienza = na.locf(provenienza),
#          sacco = na.locf(sacco)) %>% 
#   group_by(specie,materiale) %>% 
#   select(-campioni_conf)

dati <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) #più ordinato di versione precedente

# n_distinct(dati$conf_orig)


# Selezionare solo sacchi già analizzati ----------------------------------

#ho il problema del sacco delle Trachee, quindi lo aggiro chiamandolo sacco 0

dati <- 
  dati %>% mutate(sacco= ifelse(sacco =="T", 0, as.numeric(sacco)))

# view(dati)
# dati <- dati %>% filter(sacco < 146) # | sacco>=112 & sacco <=114)

# dicembre 2024: non mi interessa più perché è analisi complessiva, non parziale


# Preparazione dati per analisi --------------------------------------------
str(dati)

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

unique(dt_an$specie)

# Tabella frequenza per sezione e specie -----------------------------------------




conf_prov <- dt_an %>%
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
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:17)) #filter(Pos_Pancov == "Pos") %>%  view()



  
prov <- table(conf_prov$provenienza)
prov2 <- as.data.frame(prov)

tab_prov <- prov2 %>% add_column(percent = as.numeric(round(prov/length(conf_prov$provenienza)*100, 1))) %>% 
  dplyr::rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% 
  janitor::adorn_totals()

tab_prov %>% write.xlsx(file = here("2024.12.14 Summary Animali per Sezione.xlsx"))


#vorrei cercare di raggrupparli per specie e provincia

specie_prov <-  conf_prov %>% 
  group_by(specie, provenienza) %>%
  summarise(Conteggio_Totali = n(), .groups = "drop") 


specie_prov_export <- specie_prov %>% 
  pivot_wider(names_from = provenienza, values_from = Conteggio_Totali, values_fill = 0) %>%
  janitor::adorn_totals(where = c("row","col"))     # Conta le righe per ogni combinazione

specie_prov_export %>% write.xlsx(file = here("2024.12.17 Tabella campionamenti per specie e provincia.xlsx"))

#Ma pure positivi per sezione.

pos_prov <- conf_prov %>% filter(Pos_Pancov == "Pos")

prov3 <- table(pos_prov$provenienza)
prov4 <- as.data.frame(prov3)

tab_pos_prov <- prov4 %>% add_column(percent = as.numeric(round(prov3/length(pos_prov$provenienza)*100, 1))) %>% 
  dplyr::rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% 
  janitor::adorn_totals()


tab_pos_prov %>% write.xlsx(file = here("2024.12.14 Summary Animali positivi per Sezione.xlsx"))

#ora faccio la stessa conta per provincia e specie, ma sui positivi pancov

pos_specie_prov <- pos_prov %>% 
  group_by(specie, provenienza) %>%
  summarise(Conteggio_Positivi = n(), .groups = "drop") # %>% 
 

pos_specie_prov_export <- pos_specie_prov %>% 
  pivot_wider(names_from = provenienza, values_from = Conteggio_Positivi, values_fill = 0) %>%
  janitor::adorn_totals(where = c("row","col"))
  

pos_specie_prov_export %>% write.xlsx(file = here("2024.12.17 Tabella positivi pancov per specie e provincia.xlsx"))



# Unisci le due tabelle, mantenendo tutti i dati dei positivi e aggiungendo i totali
tabella_prevalenza <- pos_specie_prov %>%
  left_join(specie_prov, by = c("specie", "provenienza")) %>%
  # Calcola la prevalenza
  mutate(Prevalenza = case_when(
    is.na(Conteggio_Totali) ~ NA_real_,  # Se non ci sono totali (combinazione mancante), metti NA
    Conteggio_Totali == 0 ~ NA_real_,    # Se i totali sono 0, metti NA
    TRUE ~ round(Conteggio_Positivi / Conteggio_Totali * 100, 2)  # Calcola la prevalenza
  )) %>%
  select(specie, provenienza, Prevalenza)

# Rimetti i dati in formato largo (pivot_wider)
tabella_prevalenza_wide <- tabella_prevalenza %>%
  pivot_wider(names_from = provenienza, values_from = Prevalenza, values_fill = list(Prevalenza = NA))

# Visualizza il risultato
view(tabella_prevalenza_wide)

#esporta risultato

tabella_prevalenza_wide %>% write.xlsx(file = here("2024.12.17 Tabella prevalenza pancov per provincia.xlsx"))



# Mappa conferimenti per sezione ------------------------------------------

library(sf)
library(tmap)    # for static and interactive maps


ER <- st_read("dati/limits_R_8_municipalities.geojson")

ER2 <- ER %>% 
  mutate(Sede_IZSLER = if_else(prov_name %in% c("Piacenza", "Parma", "Reggio nell'Emilia", "Modena", "Bologna", "Ravenna", "Ferrara"), prov_name, "Forlì"), .after= minint_finloc)
 


#identifying branches by color

branch_colors <- scale_fill_manual(values = grey.colors(n = length(unique(ER2$Sede_IZSLER))))

# Calculate center of each province

# Convert dataframe in sf object
ER2_sf <- st_as_sf(ER2)

# Group by branch and summarize municipalities' geometry by branch
province_geom <- ER2_sf %>%
  group_by(Sede_IZSLER) %>%
  summarize(geometry = st_union(geometry))

# Calculate province centroids
province_centroids <- province_geom %>%
  st_centroid()


#definire numero di campioni per sede

tab_prov2 <- tab_prov %>% select(-(3)) %>%
  rename(Sede_IZSLER = Provincia) %>%
  mutate(Sede_IZSLER = as.character(Sede_IZSLER)) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "FO", "Forlì")) %>%
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "MO", "Modena")) %>%
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "BO", "Bologna")) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "RE", "Reggio nell'Emilia")) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "PR", "Parma")) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "PC", "Piacenza")) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "FE", "Ferrara")) %>% 
  mutate(Sede_IZSLER = replace(Sede_IZSLER, Sede_IZSLER %in% "RA", "Ravenna"))
  
tab_pos_prov2 <- tab_pos_prov %>% 
  filter(Provincia != "Total") %>%
  select(-(3))

province_geom <- province_geom %>% 
  left_join(tab_prov2, by="Sede_IZSLER") %>% 
  add_column(., prov_acr = c("BO","FE","FO","MO","PR","PC","RA","RE"))



province_geom <- province_geom  %>% 
  mutate(positivi = if_else(Sede_IZSLER=="Bologna", 23, 
                            if_else(Sede_IZSLER== "Forlì", 16, 
                                    if_else(Sede_IZSLER=="Reggio nell'Emilia", 15, 
                                            if_else(Sede_IZSLER=="Ferrara", 14,
                                                    if_else(Sede_IZSLER=="Piacenza", 9,
                                                            if_else(Sede_IZSLER=="Parma", 8,
                                                                    if_else(Sede_IZSLER=="Modena", 6,0))))))))

province_geom <- province_geom %>% 
  mutate(label= paste(prov_acr, Totale, sep=" ")) %>% 
  mutate(label_pos = paste(prov_acr, positivi, sep=" "))
  
 


mappa_camp_sez <- tm_shape(province_geom) +
  tm_polygons("Totale", fill.scale = tm_scale_categorical(values = "greens", values.range = c(0.1, 0.7)), fill.legend = tm_legend_hide())+
  tm_shape(province_geom) + # Aggiungi le etichette per le province
  tm_text("label", size = 1, col = "black", fontface = "bold")+
  tm_layout(legend.position = c("left", "bottom"), legend.title.size = 0.6, legend.text.size = 0.6)

mappa_pos_sez <- tm_shape(province_geom) +
  tm_polygons("positivi", fill.scale = tm_scale_categorical(values = "oranges", values.range = c(0.1, 0.7)), fill.legend = tm_legend_hide())+
  tm_shape(province_geom) + # Aggiungi le etichette per le province
  tm_text("label_pos", size = 1, col = "black", fontface = "bold")+
  tm_layout(legend.position = c("left", "bottom"), legend.title.size = 0.6, legend.text.size = 0.6)

tmap_save(mappa_camp_sez, filename = "Mappa campionamenti per provincia.png", dpi =600)
tmap_save(mappa_pos_sez, filename = "Mappa positivi per sezione.png", dpi = 600)





# Summary positivi Pancov per specie --------------------------------------


#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

tab_pc <- dt_an %>%
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
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:17)) %>% #filter(Pos_Pancov == "Pos") %>%  view()
  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(Pos_Pancov == "Pos"), NEG= sum(Pos_Pancov == "Neg")) %>% 
  arrange(desc(N_individui))

view(tab_pc)

library(flextable)
set_flextable_defaults(background.color = "white") #serve per evitare trasparenza!

   
ft_pancov <- tab_pc %>% 
  janitor::adorn_totals("row", name = "TOTALE") %>% #adorn_totals aggiunge riga o colonna con i totali! 
  flextable() %>% autofit() %>% set_header_labels(.,specie="SPECIE", N_individui="INDIVIDUI") %>% set_caption(caption = as_paragraph(as_b('Results of the Pan-Coronavirus analysis.'))) %>%  hline(., i = 33, part = "body") #ultimo comando aggiunge linea prima di ultima riga (dopo la 22)

ft_pancov %>% save_as_image(., here("exports", "Tabella Pancov 2024 ita.png"), expand=15, res = 200) #se non si setta background bianco prima sarà trasparente di default!


fig1 <- tab_pc %>% filter(NEG > 1) %>% select(-2) %>% pivot_longer(!specie, names_to = "PanCoV", values_to = "totali") %>%
  ggplot()+
  aes(x=fct_reorder(specie, totali, .desc = T), y=totali, fill=PanCoV)+
  geom_col(position = "stack")+
  geom_text(aes(specie, label = ifelse(totali>0, totali,"")), position = position_stack(vjust = 1), size=5, hjust = 0.5, vjust = -0.5)+#, position = position_dodge(width = 1))+
  labs(x="Specie", y="Totali")+
  theme_minimal()+
  theme(axis.text = element_text(size=12 ,face = "bold"),
        legend.text = element_text(size=12 ,face = "bold"),
        axis.text.x.bottom = element_text(vjust= 12),
        legend.title = element_text(size=12, face = "bold"))+
  coord_flip()


fig2 <- 
  tab_pc %>% filter(NEG > 1) %>% select(-2) %>% pivot_longer(!specie, names_to = "PanCoV", values_to = "totali") %>% 
  mutate(totali = as.numeric(totali)) %>%
  ggplot()+
  aes(x= totali, y=fct_reorder(specie, totali, .desc = F), fill= PanCoV)+
  geom_col(position = "stack")+
  labs(x="Totali", y="Specie")+
  geom_text(aes(totali, label = ifelse(totali>0, totali,"")), position = position_stack(vjust = 1), size=5, hjust = -0.1, vjust = 0.5)+#, position = position_dodge(width = 1))+
  theme_minimal()+
  theme(axis.text = element_text(size=12 ,face = "bold"),
        legend.text = element_text(size=12 ,face = "bold"),
        axis.text.x.bottom = element_text(vjust= 4),
        legend.title = element_text(size=12, face = "bold"),
        axis.title = element_text(size=12, face= "bold"),
        axis.title.y = element_text(vjust = 2),
        legend.position = "bottom",
        panel.background = element_rect(fill= "azure", colour = "white"))
        #panel.border = element_blank())


tab_pc %>% write.xlsx(file = here("2024.12.13 Summary positivi Pancov per specie ita.xlsx"))

tab_pc %>% summarise(Totali=sum(N_individui), Positivi=sum(POS), Negativi=sum(NEG)) %>% view()  


# Summary positivi pancov per organo ---------------------------------------------

org_pc <- dt_an %>% 
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         pancov = ifelse(pancov=="POS",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = pancov, values_fill = 0) %>% 
  select(-progr) %>% 
  select(-sacco, -piastra_estrazione, -data_esito,-note) %>%
  group_by(conf_orig, specie,provenienza, anno) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>%  #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>% 
  janitor::adorn_totals() %>% 
  filter(conf_orig == "Total") %>% 
  select(-(1:4)) %>% #filter(Pos_Pancov == "Pos") %>%  view()
  mutate(feci_retto= rowSums(select(.,2,4,6))) %>%
  select(-c(2,4,6)) %>%
  select(-9) %>% 
  rename(FECI_RETTO = feci_retto) %>%
  pivot_longer(cols = 1:10, names_to = "Organi", values_to = "Totale") %>% 
  arrange(desc(Totale))

view(org_pc)
org_pc %>% write.xlsx(file = here("2024.12.13 Summary positivi Pancov per organo ita.xlsx"))
            

# Summary positivi SARS-CoV-2 per specie ----------------------------------

tab_SC2 <- dt_an %>%
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         esito = ifelse(esito=="Positivo",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = esito, values_fill = 0) %>% 
  select(-progr) %>% 
  select(-sacco, -piastra_estrazione, -data_esito,-note) %>%
  group_by(conf_orig, specie,provenienza, anno) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>%
  mutate(somma = sum(c_across(c(5:17))),
         Pos_SARS_CoV_2 = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:17)) %>% #filter(Pos_Pancov == "Pos") %>%  view()
  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(Pos_SARS_CoV_2 == "Pos"), NEG= sum(Pos_SARS_CoV_2 == "Neg")) %>% 
  arrange(desc(N_individui))

view(tab_SC2)

tab_SC2 %>% write.xlsx(file = here("2024.12.13 Summary positivi SARS-CoV-2 per Specie.xlsx"))

# Sierologia --------------------------------------------------------------

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


#voglio fare tabella tipo tab_pc per ELISA ed sELISA, poi aggiungerla a tab_pc

view(sieri)
unique(sieri$elisa)


tab_sieri_2 <- sieri %>% mutate(elisa = replace(elisa, elisa %in% c("INSUFF", "ASSENTE"), NA)) %>% 
  filter(!is.na(elisa)) %>% filter(specie != c("GATTO","CINGHIALE")) %>% filter(! (conf_orig==32419 & elisa=="NEG")) %>% #il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  filter(! (conf_orig==32422 & s_elisa=="NEG")) %>% #il conferimento 32422 è stato fatto in due aliquote, di cui solo una positiva per s_elisa
  mutate(elisa = ifelse(elisa=="POS", 1, 0),
         s_elisa = ifelse(s_elisa=="POS", 1, 0), 
         materiale = ifelse(materiale %in% c("UMOR VITREO", "UMOR"), "UMOR ACQUEO", materiale)) %>% 
  mutate(materiale = ifelse(materiale %in% c("UMOR ACQUEO", "MUSCOLO"), materiale, "SIERO") ,
         specie = replace(specie, specie %in% "CAPRIOLO", "ROE DEER"),
         specie = replace(specie, specie %in% "DAINO", "FALLOW DEER"),
         specie = replace(specie, specie %in% "TASSO", "BADGER"),
         specie = replace(specie, specie %in% "ISTRICE", "PORCUPINE"),
         specie = replace(specie, specie %in% "LEPRE", "HARE"),
         specie = replace(specie, specie %in% "RICCIO", "HEDGEHOG"),
         specie = replace(specie, specie %in% "SCOIATTOLO", "SQUIRREL"),
         specie = replace(specie, specie %in% "LUPO", "WOLF"),
         specie = replace(specie, specie %in% "CERVO", "DEER"),
         specie = replace(specie, specie %in% "FAINA", "BEECH MARTEN"),
         specie = replace(specie, specie %in% "DELFINO", "TURSIOPS"),
         specie = replace(specie, specie %in% "VOLPE", "RED FOX"),
         specie = replace(specie, specie %in% "SILVILAGO", "MARSH RABBIT"),
         specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
         specie = replace(specie, specie %in% "CANGURO", "WALLABY"),
         specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
         specie = replace(specie, specie %in% "CONIGLIO", "RABBIT"),
         specie = replace(specie, specie %in% "LONTRA", "OTTER"),
         specie = replace(specie, specie %in% "SURICATO", "MEERKAT"),
         specie = replace(specie, specie %in% "AGUTI DI AZARA", "AZARA'S AGOUTI"),
         specie = replace(specie, specie %in% "RATTO", "RAT"),
         specie = replace(specie, specie %in% "TOPO", "MOUSE"),
         specie = replace(specie, specie %in% "GHIRO", "DORMOUSE"),
         specie = replace(specie, specie %in% "GATTO SELVATICO", "EUROPEAN WILDCAT")) %>%
  distinct(conf_orig, .keep_all = T) %>%
pivot_wider(names_from = materiale, values_from = elisa, values_fill = 0) %>%
select(-percent_pos) %>%
group_by(conf_orig, specie) %>% 
summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>%
  ungroup() %>% 
  #select(-sacco) %>% 
  rowwise() %>% 
  mutate(elisa = ifelse(sum(c_across(c(4:5))) >=1, "Pos", "Neg"),
         s_elisa = ifelse(s_elisa==1, "Pos", "Neg")) %>% 
select(-SIERO, -`UMOR ACQUEO`) %>%
relocate(elisa, .before = s_elisa) %>% 
  group_by(specie) %>% 
  summarise(Totale = n(), elisa = sum(elisa == "Pos"), s_elisa= sum(s_elisa == "Pos"), .groups = "drop") %>%  #%>% #filter(.,Totale>10) %>% 
    #filter(., elisa>0) %>% 
  #arrange(desc(elisa)) %>% 
janitor::adorn_totals("row", name = "TOTAL") #adorn_totals aggiunge riga o colonna con i totali!

# Tab sieri modificata per EUSV 2025 --------------------------------------

tab_sieri <- sieri %>% 
  mutate(elisa = replace(elisa, elisa %in% c("INSUFF", "ASSENTE"), NA)) %>% 
  filter(!is.na(elisa)) %>% 
  filter(!specie %in% c("GATTO")) %>% #ho rimosso il filtro sulla specie cinghiale, perché uno è stato testato. sopra non funzionava e lo contava, quindi non tornavano i numeri
  filter(!(conf_orig == 32419 & elisa == "NEG")) %>% # il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  filter(!(conf_orig == 32422 & s_elisa == "NEG")) %>% # il conferimento 32422 è stato fatto in due aliquote, di cui solo una positiva per s_elisa
  mutate(
    elisa = ifelse(elisa == "POS", 1, 0),
    s_elisa = ifelse(s_elisa == "POS", 1, 0),
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
                    "GATTO SELVATICO" = "EUROPEAN WILDCAT")
  ) %>%
  distinct(conf_orig, .keep_all = TRUE) %>%
  pivot_wider(names_from = materiale, values_from = elisa, values_fill = 0) %>%
  select(-percent_pos) %>%
  group_by(conf_orig, specie) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  rowwise() %>% 
  mutate(
    elisa = ifelse(sum(c_across(c(4:5))) >= 1, "Pos", "Neg"),
    s_elisa = ifelse(s_elisa == 1, "Pos", "Neg")
  ) %>%
  select(-SIERO, -`UMOR ACQUEO`) %>%
  relocate(elisa, .before = s_elisa) %>%
  group_by(specie) %>% 
  summarise(
    Totale = n(),
    elisa = sum(elisa == "Pos"),
    s_elisa = sum(s_elisa == "Pos"),
    .groups = "drop"
  ) %>%
  # Collassa tutte le specie con Totale <=10 e/o elisa = 0 in una sola riga "OTHER SPECIES"
  mutate(specie = ifelse(elisa == 0, "OTHER SPECIES", specie)) %>%
  group_by(specie) %>%
  summarise(
    Totale = sum(Totale),
    elisa = sum(elisa),
    s_elisa = sum(s_elisa),
    .groups = "drop"
  ) %>%
  mutate(ord = ifelse(specie == "OTHER SPECIES", 1, 0)) %>% # 1 = la mettiamo dopo
  arrange(ord, desc(elisa), desc(Totale)) %>%
  select(-ord) %>% 
janitor::adorn_totals("row", name = "TOTAL") #adorn_totals aggiunge riga o colonna con i totali!





tab_sieri %>%  write.xlsx(file = here("2024.12.17 Summary sieri positivi SARS-CoV-2 per Specie.xlsx"))

library(flextable)
set_flextable_defaults(background.color = "white") #serve per evitare trasparenza!



ft_sieri <- tab_sieri %>% 
  flextable() %>% set_header_labels(.,specie="Species", Totale="Total", elisa="Positive for N-ELISA", s_elisa="Positive for S-ELISA")%>% #set_caption(caption = as_paragraph(as_b("Figure 2: Summary of serological analysis results: all samples were screened with an anti-N ELISA; the positive ones were tested again with an anti-S ELISA for confirmation. Species where no N ELISA tested positive are not shown."))) %>% 
  #add_footer_lines(values = "Figure 2: Summary of serological analysis results: all samples were screened with an anti-N ELISA; the positive ones were tested again with an anti-S ELISA for confirmation. Species where no N ELISA tested positive are not shown.") %>% bold(bold = T, part = "footer") %>% 
  autofit() %>%
  hline(., i = 11, part = "body") #ultimo comando aggiunge linea prima di ultima riga (dopo la 12)

sum(ft_sieri$Totali)

ft_sieri %>% 
  save_as_image(., here("exports", "Tabella sierologia EuSV 2025_rev2.png"), expand=15, res = 200) #se non si setta background bianco prima sarà trasparente di default!




# Sieri positivi per provincia e specie --------


sieri_an <- sieri %>% mutate(elisa = replace(elisa, elisa %in% c("INSUFF", "ASSENTE"), NA)) %>% 
  filter(!is.na(elisa)) %>% filter(specie != c("GATTO", "CINGHIALE")) %>% filter(! (conf_orig==32419 & elisa=="NEG")) %>% #il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  filter(! (conf_orig==32422 & s_elisa=="NEG")) %>%   #il conferimento 32422 è stato fatto in due aliquote, di cui solo una positiva per s_elisa
  mutate(elisa = ifelse(elisa=="POS", 1, 0),
         s_elisa = ifelse(s_elisa=="POS", 1, 0), 
         materiale = ifelse(materiale %in% c("UMOR VITREO", "UMOR"), "UMOR ACQUEO", materiale)) %>% 
  mutate(materiale = ifelse(materiale %in% c("UMOR ACQUEO", "MUSCOLO"), materiale, "SIERO")) %>%
  distinct(conf_orig, .keep_all = T) %>% 
  pivot_wider(names_from = materiale, values_from = elisa, values_fill = 0) %>%
  select(-percent_pos) %>%
  group_by(conf_orig, specie, provenienza) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% 
  ungroup() %>% 
  #select(-sacco) %>% 
  rowwise() %>% 
  mutate(elisa = ifelse(sum(c_across(c(4:5))) >=1, "Pos", "Neg"),
         s_elisa = ifelse(s_elisa==1, "Pos", "Neg")) %>%
  select(-SIERO, -`UMOR ACQUEO`) %>%
  relocate(elisa, .before = s_elisa)


sieri_pos_s <-  sieri_an %>% 
  filter(s_elisa == "Pos")  
  


sieri_sp_prov <- sieri_an %>% group_by(specie, provenienza) %>%
  summarise(Conteggio_Totali = n(), .groups = "drop")

sieri_sp_prov_export <- sieri_sp_prov %>% 
  pivot_wider(names_from = provenienza, values_from = Conteggio_Totali, values_fill = 0) %>%
  janitor::adorn_totals(where = c("row","col"))

sieri_sp_prov_export %>% write.xlsx(file = here("2024.12.17 Summary sieri per sezione e specie.xlsx"))

sieri_pos_sp_prov <- sieri_pos_s %>% 
  group_by(specie, provenienza) %>%
  summarise(Conteggio_Positivi = n(), .groups = "drop")

sieri_pos_sp_prov_export <- sieri_pos_sp_prov %>% 
  pivot_wider(names_from = provenienza, values_from = Conteggio_Positivi, values_fill = 0) %>%
  janitor::adorn_totals(where = c("row","col"))

sieri_pos_sp_prov_export %>% write.xlsx(file = here("2024.12.17 Summary sieri positivi S per sezione e specie.xlsx"))

# Unisci le due tabelle, mantenendo tutti i dati dei positivi e aggiungendo i totali
prevalenza_sieri <- sieri_pos_sp_prov %>%
  left_join(sieri_sp_prov, by = c("specie", "provenienza")) %>%
  # Calcola la prevalenza
  mutate(Prevalenza = case_when(
    is.na(Conteggio_Totali) ~ NA_real_,  # Se non ci sono totali (combinazione mancante), metti NA
    Conteggio_Totali == 0 ~ NA_real_,    # Se i totali sono 0, metti NA
    TRUE ~ round(Conteggio_Positivi / Conteggio_Totali * 100, 2)  # Calcola la prevalenza
  )) %>%
  select(specie, provenienza, Prevalenza)

# Rimetti i dati in formato largo (pivot_wider)
prevalenza_sieri_wide <- prevalenza_sieri %>%
  pivot_wider(names_from = provenienza, values_from = Prevalenza, values_fill = list(Prevalenza = NA))

# Visualizza il risultato
view(prevalenza_sieri_wide)

#esporta risultato

prevalenza_sieri_wide %>% write.xlsx(file = here("2024.12.17 Tabella prevalenza sieri per sezione e specie.xlsx"))

  
  






# Confronto sieri vs pancov -----------------------------------------------

Pancov <- dt_an %>%
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
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:17)) %>% 
  filter(Pos_Pancov == "Pos") %>%
  group_by(conf_orig,anno)

unique(Pancov$conf_orig)

view(sieri)

sieri_pos <-  sieri_an %>% 
  filter(elisa == "Pos") 


Cross_reattività <- as.data.frame(intersect(unique(Pancov$conf_orig),unique(sieri_pos$conf_orig))) %>% 
  rename("conf_orig" = "intersect(unique(Pancov$conf_orig), unique(sieri_pos$conf_orig))")

Pancov_filtrato_sieri <- Pancov %>% filter(conf_orig %in% sieri_pos$conf_orig) 

#pare che non ci sia cross-reattività



#calcolo intervalli di confidenza

binom.test(14,71)

































