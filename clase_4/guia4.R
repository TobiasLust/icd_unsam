library(tidyverse)

df <- read_csv('arbolado_comuna14.csv')

df_filt <- df %>% filter(nombre_cientifico %in%
                           c('Tilia x moltkei', 'Fraxinus excelsior', 'Melia azedarach'))

df_filt %>% group_by(nombre_cientifico)%>%
  summarise(arboles = n())

df_filt %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol,color=nombre_cientifico))+
  geom_jitter(height = 0)+
  theme_minimal()

df_resumen <- df_filt %>%
  group_by(nombre_cientifico)%>%
  summarise(prom_alt = mean(altura_arbol,na.rm=T))%>%
  arrange(desc(prom_alt))

<<<<<<< HEAD
df <- df%>%group_by(nombre_cientifico)%>%
  mutate(altura_max = max(altura_arbol,na.rm=T))

max_fraxinus <- df[df$nombre_cientifico=='Fraxinus excelsior',]$altura_max[1]
nuevo_df <- mutate(df,altura_arbol = if_else(altura_arbol == max_fraxinus &
                                               nombre_cientifico == 'Fraxinus excelsior',
                                             max_fraxinus/10,altura_arbol) )
            
                                             

df_altura_max <- nuevo_df %>% group_by(nombre_cientifico) %>%
  summarise(altura_max= max(altura_arbol,na.rm=T))%>%
  arrange(desc(altura_max))


nuevo_df <- nuevo_df %>%
  group_by(nombre_cientifico) %>%
  mutate(altura_max = max(altura_arbol, na.rm = TRUE))


quantile(df$altura_arbol,probs = c(0.05,0.5,0.95),na.rm=T)

df_resumen <- df %>% group_by(nombre_cientifico)%>%
  summarise(Q5 = quantile(altura_arbol,probs=0.05,na.rm=T),
            Q95 = quantile(altura_arbol,probs=0.95,na.rm=T)
            ,max=max(altura_arbol,na.rm=T),min=min(altura_arbol,na.rm=T))

df_resumen_iqr <- df %>% group_by(nombre_cientifico)%>%
  summarise(Q1=quantile(altura_arbol,probs=0.25,na.rm=T),
            Q3=quantile(altura_arbol,probs=0.75,na.rm=T),
            IQR=IQR(altura_arbol,na.rm=T))

df %>% 
  group_by(nombre_cientifico) %>%
  summarise(
    name = c('min', '1st', 'median', '3rd', 'max'), 
    value = fivenum(altura_arbol)
  ) %>%
  pivot_wider(names_from = name, values_from = value)



df_filt %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol,color=nombre_cientifico))+
  geom_jitter(height = 0)+
  geom_boxplot(alpha=0.4)+
  geom_hline(yintercept = quantile(df$altura_arbol, probs = c(0.25, 0.50, 0.75), na.rm = TRUE), 
             linetype = "dashed", color = "red")+
  theme_minimal()

df_filt<-df_filt %>%
  group_by(nombre_cientifico)%>%
  mutate(outlier_down = quantile(altura_arbol,0.25,na.rm=T) - (IQR(altura_arbol,na.rm=T)*1.5),
         outlier_up = quantile(altura_arbol,0.75,na.rm=T) + (IQR(altura_arbol,na.rm=T)*1.5))

df_filt <- df_filt %>%
  mutate(outlier = altura_arbol < outlier_down | altura_arbol > outlier_up) %>%
  group_by(nombre_cientifico) %>%
  summarise(n_outliers = sum(outlier, na.rm = TRUE))


df_filt %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol,fill = nombre_cientifico))+
  geom_violin(draw_quantiles =
                c(0.25, 0.5, 0.75))+
  theme_minimal() +
  labs(title = "Distribución de Alturas de Árboles por Especie",
       x = "Especie",
       y = "Altura del Árbol (m)") +
  scale_fill_brewer(palette = "Set2")
=======

df_n_cientf %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol))+
  geom_jitter(width = 0.3, height = 0.3)

df_resumen <- df_n_cientf%>%
  group_by(nombre_cientifico)%>%
  summarise(prom = mean(altura_arbol, na.rm=T), altura_max = max(altura_arbol,na.rm=T))


max_fraxinus <- df_resumen[df_resumen$nombre_cientifico=='Fraxinus excelsior',]$altura_max


new_df <-  mutate(df,altura_arbol = if_else(altura_arbol == max_fraxinus &
                                             nombre_cientifico == 'Fraxinus excelsior',
                                           max_fraxinus/10,
                                           altura_arbol) )


df_new <- new_df%>%
  group_by(nombre_cientifico)%>%
  summarise(prom = mean(altura_arbol, na.rm=T), altura_max = max(altura_arbol,na.rm=T))


#mediana
quantile(df$altura_arbol, probs = c(0.05, 0.5, 0.95), na.rm=TRUE)


df_especie <- df %>%
  group_by(nombre_cientifico)%>%
  summarise(altura_max = max(altura_arbol,na.rm=T),cuantil_05=quantile(altura_arbol,probs=0.5,na.rm=T),cuantil_095=quantile(altura_arbol,probs=0.95,na.rm=T))



media <- df %>% group_by(nombre_cientifico) %>%
  summarise(name = c('min', '1st', 'median', '3rd', 'max'), value =
              fivenum(altura_arbol))%>%
  pivot_wider(names_from=name, values_from=value)


df_n_cientf %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol))+
  geom_boxplot(aes(alpha = 0.4))

df_n_cientf %>%
  ggplot(aes(x=nombre_cientifico,y=altura_arbol))+
  geom_violin(draw_quantiles =
                c(0.25, 0.5, 0.75))
>>>>>>> f4b1096 (clase5)
