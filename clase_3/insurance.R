library(tidyverse)
insurance_df <- read_csv('insurance.csv')


# df con solo hombres fumadores
df_male_smokers <- insurance_df %>%
  filter(sex=='male' & smoker=='yes') # nos quedamos con 159 filas/observaciones

insurance_df %>%
  ggplot()+geom_point(aes(x=age,y=charges,color=smoker))

df_male_smokers %>%
  ggplot(aes(x=charges)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_dotplot(binwidth=5000, 
               fill='salmon', color='transparent',
               dotsize=0.2) +
  # Para sacar las etiquetas del eje y, que no indican nada
  #scale_y_continuous(name=NULL, labels=NULL) +
  labs(x="Cargos medicos",
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=12))


insurance_df %>%
  ggplot(aes(x=charges)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_dotplot(binwidth=5000, 
               fill='salmon', color='transparent',
               dotsize=0.2) +
  # Para sacar las etiquetas del eje y, que no indican nada
  #scale_y_continuous(name=NULL, labels=NULL) +
  labs(x="Cargos medicos",
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=12))


df_male_smokers %>%
  ggplot(aes(x=charges)) +
  geom_histogram(binwidth=5000) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name='Cantidad personas') +
  labs(x="Cargos medicos",
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=12))



insurance_df %>%
  ggplot(aes(x=charges)) +
  geom_histogram(binwidth=7000) +
  scale_y_continuous(name='Cantidad personas') +
  labs(x="Cargos medicos",
       title='Costo del seguro de salud') +
  theme(axis.title = element_text(size=12))


bw <- 5000
  insurance_df %>% 
  filter(smoker=='yes', sex=='male') %>%
  ggplot(aes(x=charges)) +
  # Grafica el histograma
  geom_histogram(binwidth=bw, 
                 fill='darkgreen', color='transparent') +
  stat_bin(aes(y=..count.., label=..count..), binwidth=bw,
           geom='text', vjust=-.5) +
  labs(y=???, x='Costo del seguro [USD]')
