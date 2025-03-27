library(tidyverse)
insurance_df <- read_csv('insurance.csv')


# df con solo hombres fumadores
df_male_smokers <- insurance_df %>%
  filter(sex=='male' & smoker=='yes') # nos quedamos con 159 filas/observaciones

df_male_smokers %>%
  ggplot(aes(x=charges)) +
  geom_histogram(binwidth=5000, 
               fill='salmon')  +
  labs(x='Cargos [USD]',
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

insurance_df %>%
  ggplot(aes(x=charges))+
  geom_histogram(binwidth = 10000)


insurance_df %>%
  filter(smoker=='yes')%>%
    ggplot(aes(x=charges))+
    geom_histogram(binwidth = 5000)


bw <- 10000
  insurance_df %>% 
  filter(smoker=='yes', sex=='male') %>%
  ggplot(aes(x=charges)) +
  # Grafica el histograma
  geom_histogram(binwidth=bw, 
                 fill='darkgreen', color='transparent') +
  stat_bin(aes(y=..count.., label=..count..), binwidth=bw,
           geom='text', vjust=-.5) +
  labs(y='cantidad', x='Costo del seguro [USD]')
  
  
  insurance_df %>% 
    filter(sex=='male') %>%
    ggplot(aes(x=charges, fill=smoker)) +
    # Completen la posición. Vean la diferencia entre 'stack' y 'identity'
    geom_histogram(binwidth=bw, color='transparent', position='identity',
                   alpha=0.5) +
    density()+
    labs(y='Cuentas', x='Costo del seguro [USD]')