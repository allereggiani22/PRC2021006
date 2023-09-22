source(here('R', 'librerie.R'))


dt2022 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "numeric", "text", "text", "date", 
                                 "text", "text", "date", "text", "date", 
                                 "text", "text", "text", "skip"))

dt2023 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                                                    sheet = "Campioni PRC 2023", col_types = c("numeric", 
                                                                                               "text", "text", "numeric", "text", 
                                                                                               "text", "text", "text", "date", "text", 
                                                                                              "date", "text", "text", "text", "skip"))



dt2023 <- add_column(dt2023,Anno= 2023, .after="Materiale")
dt2022 <- dt2022 %>% dplyr::rename(Anno=Data) %>% mutate(Anno=2022)
dt2022 <- dt2022[-(3871:3991),]
dt2023 <- dt2023[-(3250:4023),]

dati <- bind_rows(dt2022,dt2023)

dati <- clean_names(dati)
#view(dati)


#unique(dati$Specie)
#conferimenti <- unique(dati$`Conf. orig`)
#table(conferimenti)

# Riempire celle vuote con valore precedente ------------------------------


# dati_2022 <- dati %>% 
#   mutate(conf_orig = na.locf(conf_orig),
#          provenienza = na.locf(provenienza),
#          sacco = na.locf(sacco)) %>% 
#   group_by(specie,materiale) %>% 
#   select(-campioni_conf)

dati <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) %>% 
  select(-campioni_conf)  #più ordinato di versione precedente

#n_distinct(dati_2022$conf_orig)


# Selezionare solo sacchi già analizzati ----------------------------------

#ho il problema del sacco delle Trachee, quindi lo aggiro chiamandolo sacco 0

dati <- 
  dati %>% mutate(sacco= ifelse(sacco =="T", 0, as.numeric(sacco)))

dati <- dati %>% filter(sacco < 100 | sacco>=112 & sacco <=114)





# Tabella frequenza per provincia -----------------------------------------


prov <- table(dati$provenienza) #numero di campioni per sede

round(prov/length(dati$provenienza)*100, 0) #percentuale per sede

prov2 <- as.data.frame(prov)
prov2 %>% add_column(percent = round(prov/length(dati$provenienza)*100, 0)) %>% 
  rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% view()
  


# Prove analisi --------------------------------------------
str(dati)

dt_an <- dati %>%
  filter(!is.na(materiale)) %>% 
  filter(!is.na(progr)) %>% 
  filter(!is.na(specie)) %>% 
  filter(specie != "CINGHIALE")

unique(dt_an$materiale)

dt_an <- dt_an %>% 
  mutate(materiale = replace(materiale, materiale %in% "T. NAS", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. RETT", "T.RETT"),
         materiale = replace(materiale, materiale %in% c("ILEO/DIGIUNO", "DIGIUNO","DUODENO"), "INTESTINO"),
         materiale = replace(materiale, materiale %in% "SENI NASALI", "T.NAS"),
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
         specie = replace(specie, specie %in% "PROCIONE", "RACCOON"),
         specie = replace(specie, specie %in% "MARTORA", "PINE MARTEN"),
         specie = replace(specie, specie %in% "FURETTO", "FERRET"),
         specie = replace(specie, specie %in% "DONNOLA", "WEASEL"),
         specie = replace(specie, specie %in% "TALPA", "MOLE"),
         specie = replace(specie, specie %in% "SCIACALLO", "JACKAL"),
         specie = replace(specie, specie %in% "CINCILLA'", "CHINCHILLA"))

unique(dt_an$specie)


#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

tab_pc <- dt_an %>% 
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         pancov = ifelse(pancov=="POS",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = pancov, values_fill = 0) %>% 
  select(-progr, -conf_mo) %>% 
  select(-sacco, -piastra_estrazione, -data_esito, -conferimento_pancov,-note) %>%
  group_by(conf_orig, specie,provenienza, anno) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>% 
  mutate(somma = sum(c_across(c(5:18))),
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:18)) %>% #filter(Pos_Pancov == "Pos") %>%  view() 
  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(Pos_Pancov == "Pos"), NEG= sum(Pos_Pancov == "Neg")) %>% 
  arrange(desc(N_individui))
   
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


#fig2 <- 
  tab_pc %>% filter(NEG > 1) %>% select(-2) %>% pivot_longer(!specie, names_to = "PanCoV", values_to = "totali") %>% 
  mutate(totali = as.numeric(totali)) %>%
  ggplot()+
  aes(x= totali, y=fct_reorder(specie, totali, .desc = F), fill= PanCoV)+
  geom_col(position = "stack")+
  labs(x="Total", y="Species")+
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


#tab_pc %>% write.xlsx(file = here("2023.07.05 Summary positivi Pancov per specie.xlsx"))

#tab_pc %>% summarise(Totali=sum(N_individui), Positivi=sum(POS), Negativi=sum(NEG)) %>% view()  
            


# Sierologia --------------------------------------------------------------

sieri2022 <-  read_excel("dati/23062023 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2022", col_types = c("skip", 
                                                             "text", "skip", "text", "text", "skip", 
                                                             "skip", "skip", "skip", "text", "text", 
                                                             "numeric", "text"))

sieri2023 <-  read_excel("dati/23062023 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2023", col_types = c("text", 
                                                             "skip", "text", "text", "skip", "skip", 
                                                             "skip", "text", "text", "numeric", 
                                                             "text", "text"))
sieri2023 <- sieri2023[-194,] %>% select(-8)
sieri <- bind_rows(sieri2022, sieri2023) #ho rimosso dall'excel le S e le U dai nconf...


#voglio fare tabella tipo tab_pc per ELISA ed sELISA, poi aggiungerla a tab_pc

view(sieri)
unique(sieri$ELISA)
tab_sieri <- sieri %>% mutate(ELISA = replace(ELISA, ELISA %in% c("INSUFF", "ASSENTE"), NA)) %>% 
  filter(!is.na(ELISA)) %>% filter(Specie != "GATTO") %>% filter(! (`Conf. orig`==32419 & ELISA=="NEG")) %>% #il conferimento 32419 risultava 1 volta pos ed 1 neg su 2 campioni di siero. distinct() teneva solo il negativo, per cui l'ho rimosso. devo trovare alternativa per il futuro.
  mutate(ELISA = ifelse(ELISA=="POS", 1, 0),
         sELISA = ifelse(sELISA=="POS", 1, 0), 
         Materiale = ifelse(Materiale %in% c("UMOR VITREO", "UMOR"), "UMOR ACQUEO", Materiale)) %>% 
  mutate(Materiale = ifelse(Materiale %in% c("UMOR ACQUEO", "MUSCOLO"), Materiale, "SIERO"),
         Specie = replace(Specie, Specie %in% "CAPRIOLO", "ROE DEER"),
         Specie = replace(Specie, Specie %in% "DAINO", "FALLOW DEER"),
         Specie = replace(Specie, Specie %in% "TASSO", "BADGER"),
         Specie = replace(Specie, Specie %in% "ISTRICE", "PORCUPINE"),
         Specie = replace(Specie, Specie %in% "LEPRE", "HARE"),
         Specie = replace(Specie, Specie %in% "RICCIO", "HEDGEHOG"),
         Specie = replace(Specie, Specie %in% "SCOIATTOLO", "SQUIRREL"),
         Specie = replace(Specie, Specie %in% "LUPO", "WOLF"),
         Specie = replace(Specie, Specie %in% "CERVO", "DEER"),
         Specie = replace(Specie, Specie %in% "FAINA", "BEECH MARTEN"),
         Specie = replace(Specie, Specie %in% "DELFINO", "TURSIOPS"),
         Specie = replace(Specie, Specie %in% "VOLPE", "RED FOX"),
         Specie = replace(Specie, Specie %in% "SILVILAGO", "MARSH RABBIT"),
         Specie = replace(Specie, Specie %in% "PUZZOLA", "SKUNK"),
         Specie = replace(Specie, Specie %in% "CANGURO", "WALLABY"),
         Specie = replace(Specie, Specie %in% "PUZZOLA", "SKUNK"),
         Specie = replace(Specie, Specie %in% "CONIGLIO", "RABBIT"),
         Specie = replace(Specie, Specie %in% "LONTRA", "OTTER"),
         Specie = replace(Specie, Specie %in% "SURICATO", "MEERKAT"),
         Specie = replace(Specie, Specie %in% "AGUTI DI AZARA", "AZARA'S AGOUTI"),
         Specie = replace(Specie, Specie %in% "RATTO", "RAT"),
         Specie = replace(Specie, Specie %in% "TOPO", "MOUSE"),
         Specie = replace(Specie, Specie %in% "GHIRO", "DORMOUSE")) %>%
  distinct(`Conf. orig`, .keep_all = T) %>%
pivot_wider(names_from = Materiale, values_from = ELISA, values_fill = 0) %>%
select(-NOTE, -`% POS`) %>%
group_by(`Conf. orig`, Specie) %>% 
summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(ELISA = ifelse(sum(c_across(c(4:5))) >=1, "Pos", "Neg"),
         sELISA = ifelse(sELISA==1, "Pos", "Neg")) %>%
select(-SIERO, -`UMOR ACQUEO`) %>%
relocate(ELISA, .before = sELISA) 
  
library(flextable)
set_flextable_defaults(background.color = "white") #serve per evitare trasparenza!


ft_sieri <- tab_sieri %>% 
  group_by(Specie) %>% 
  summarise(Total = n(), ELISA = sum(ELISA == "Pos"), sELISA= sum(sELISA == "Pos")) %>% filter(.,Total>10) %>% 
  arrange(desc(ELISA)) %>% janitor::adorn_totals("row", name = "TOTALS") %>% #adorn_totals aggiunge riga o colonna con i totali!
  flextable() %>% autofit() %>% set_header_labels(.,Specie="Species") %>% set_caption(caption = as_paragraph(as_b('Table 1: Results of the serological analysis. Species with less than 10 only negative subjects were excluded.'))) %>%  hline(., i = 10, part = "body") #ultimo comando aggiunge linea prima di ultima riga (dopo la 22)


#ft_sieri %>% set_caption(caption = "Tabella 1: Tabella riassuntiva delle specie sottoposte ad analisi sierologica per SARS-CoV-2. Nelle diverse colonne sono indicati il numero di soggetti analizzati in totale e quelli risultati positivi rispettivamente all’ELISA anti-N ed alla sELISA anti-S.")

sum(ft_sieri$Totali)

ft_sieri %>% 
save_as_image(., here("exports", "Prova export.png"), expand=15, res = 200) #se non si setta background bianco prima sarà trasparente di default!



sum(ft_sieri$ELISA)
sum(ft_sieri$sELISA)

#prova commit






