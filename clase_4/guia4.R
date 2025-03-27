library(tidyverse)

df <- read_csv('arbolado_comuna14.csv')

df_n_cientf<- df %>% filter(nombre_cientifico %in%
                c('Tilia x moltkei', 'Fraxinus excelsior', 'Melia azedarach'))

# cuantos arboles de cada especie hay

df_n_cientf %>%
  group_by(nombre_cientifico)%>%
  summarise(cant_arboles = n()) 


df_n_cientf %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol))+
  geom_jitter(width = 0.3, height = 0.3)

df_resumen <- df_n_cientf%>%
  group_by(nombre_cientifico)%>%
  summarise(prom = mean(altura_arbol, na.rm=T))

df <- df%>%
  group_by(nombre_cientifico)%>%
  mutate(altura_max = max(altura_arbol,na.rm=T))
  

#14
  