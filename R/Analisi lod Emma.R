source(here('R', 'librerie.R'))

lod <- read_excel("dati/LOD per R.xlsx")

view(lod)

tab1 <- lod %>% group_by(kit, cp_reaz, target) %>% 
  summarise(Mean= mean(ct, na.rm = T), CV= (goeveg::cv(ct, na.rm=T))*100, N= n())

tab2 <- lod %>% na.omit() %>% group_by(kit, cp_reaz, target) %>% 
  summarise(Mean= mean(ct, na.rm = T), CV= (goeveg::cv(ct, na.rm=T))*100, N= n())

write.xlsx(tab2, here("exports","prova-lod2.xlsx"))

view(tab1)
view(tab2)

library(flextable)


