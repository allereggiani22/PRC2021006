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



# Tabella frequenza per provincia -----------------------------------------


prov <- table(dati$provenienza) #numero di campioni per sede

round(prov/length(dati$provenienza)*100, 0) #percentuale per sede

prov2 <- as.data.frame(prov)
prov2 %>% add_column(percent = round(prov/length(dati$provenienza)*100, 0)) %>% 
  dplyr::rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% view()
  


# Analisi --------------------------------------------
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

sieri2022 <- sieri2022 %>% select(-5)
sieri2023 <- sieri2023 %>% select(-5)
sieri2024 <- sieri2024 %>% select(-5)


sieri <- bind_rows(sieri2022, sieri2023, sieri2024) #non ho rimosso dall'excel le S e le U dai nconf...

sieri <- clean_names(sieri)
#devo rimuovere le U e le S dagli nconf, quindi
sieri <- sieri %>%
  mutate(conf_orig = str_remove(conf_orig, " [SU]$"))


#voglio fare tabella tipo tab_pc per ELISA ed sELISA, poi aggiungerla a tab_pc

view(sieri)
unique(sieri$elisa)
tab_sieri <- sieri %>% mutate(elisa = replace(elisa, elisa %in% c("INSUFF", "ASSENTE"), NA)) %>% 
  filter(!is.na(elisa)) %>% filter(specie != "GATTO") %>% filter(! (conf_orig==32419 & elisa=="NEG")) %>% #il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  mutate(elisa = ifelse(elisa=="POS", 1, 0),
         s_elisa = ifelse(s_elisa=="POS", 1, 0), 
         materiale = ifelse(materiale %in% c("UMOR VITREO", "UMOR"), "UMOR ACQUEO", materiale)) %>% 
  mutate(materiale = ifelse(materiale %in% c("UMOR ACQUEO", "MUSCOLO"), materiale, "SIERO")) %>% #,
  #        specie = replace(specie, specie %in% "CAPRIOLO", "ROE DEER"),
  #        specie = replace(specie, specie %in% "DAINO", "FALLOW DEER"),
  #        specie = replace(specie, specie %in% "TASSO", "BADGER"),
  #        specie = replace(specie, specie %in% "ISTRICE", "PORCUPINE"),
  #        specie = replace(specie, specie %in% "LEPRE", "HARE"),
  #        specie = replace(specie, specie %in% "RICCIO", "HEDGEHOG"),
  #        specie = replace(specie, specie %in% "SCOIATTOLO", "SQUIRREL"),
  #        specie = replace(specie, specie %in% "LUPO", "WOLF"),
  #        specie = replace(specie, specie %in% "CERVO", "DEER"),
  #        specie = replace(specie, specie %in% "FAINA", "BEECH MARTEN"),
  #        specie = replace(specie, specie %in% "DELFINO", "TURSIOPS"),
  #        specie = replace(specie, specie %in% "VOLPE", "RED FOX"),
  #        specie = replace(specie, specie %in% "SILVILAGO", "MARSH RABBIT"),
  #        specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
  #        specie = replace(specie, specie %in% "CANGURO", "WALLABY"),
  #        specie = replace(specie, specie %in% "PUZZOLA", "SKUNK"),
  #        specie = replace(specie, specie %in% "CONIGLIO", "RABBIT"),
  #        specie = replace(specie, specie %in% "LONTRA", "OTTER"),
  #        specie = replace(specie, specie %in% "SURICATO", "MEERKAT"),
  #        specie = replace(specie, specie %in% "AGUTI DI AZARA", "AZARA'S AGOUTI"),
  #        specie = replace(specie, specie %in% "RATTO", "RAT"),
  #        specie = replace(specie, specie %in% "TOPO", "MOUSE"),
  #        specie = replace(specie, specie %in% "GHIRO", "DORMOUSE")) %>%
  distinct(conf_orig, .keep_all = T) %>%
pivot_wider(names_from = materiale, values_from = elisa, values_fill = 0) %>%
select(-percent_pos) %>%
group_by(conf_orig, specie) %>% 
summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(elisa = ifelse(sum(c_across(c(4:5))) >=1, "Pos", "Neg"),
         s_elisa = ifelse(s_elisa==1, "Pos", "Neg")) %>%
select(-SIERO, -`UMOR ACQUEO`) %>%
relocate(elisa, .before = s_elisa) %>% 
  group_by(specie) %>% 
  summarise(Totale = n(), elisa = sum(elisa == "Pos"), s_elisa= sum(s_elisa == "Pos")) %>% filter(.,Totale>10) %>% 
  arrange(desc(elisa)) %>% janitor::adorn_totals("row", name = "TOTALE") #adorn_totals aggiunge riga o colonna con i totali!

tab_sieri %>%  write.xlsx(file = here("2024.12.13 Summary sieri positivi SARS-CoV-2 per Specie.xlsx"))
  
library(flextable)
set_flextable_defaults(background.color = "white") #serve per evitare trasparenza!



ft_sieri <- tab_sieri %>% 
  flextable() %>% autofit() %>% set_header_labels(.,specie="Specie", elisa="N ELISA", s_elisa="S ELISA") %>% set_caption(caption = as_paragraph(as_b("Risultati dell'analisi sierologica. Le specie con meno di 10 individui tutti negativi sono state escluse"))) %>%  
  hline(., i = 12, part = "body") #ultimo comando aggiunge linea prima di ultima riga (dopo la 12)


#ft_sieri %>% set_caption(caption = "Tabella 1: Tabella riassuntiva delle specie sottoposte ad analisi sierologica per SARS-CoV-2. Nelle diverse colonne sono indicati il numero di soggetti analizzati in totale e quelli risultati positivi rispettivamente all’ELISA anti-N ed alla sELISA anti-S.")

sum(ft_sieri$Totali)

ft_sieri %>% 
save_as_image(., here("exports", "Tabella sieri dicembre 2024.png"), expand=15, res = 200) #se non si setta background bianco prima sarà trasparente di default!



sum(ft_sieri$ELISA)
sum(ft_sieri$s_elisa)

#prova commit






