library(tidyverse)
library(ggridges)

insurance_df <- read_csv('insurance.csv')

insurance_male_smoker <- insurance_df %>%
  filter(sex=='male' &  smoker=='yes')

insurance_male_smoker %>%
  ggplot(aes(x=age,y=charges))+
  geom_jitter()+
  geom_smooth(se=FALSE)

insurance_male_smoker %>%
  ggplot(aes(x=charges)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_dotplot(binwidth=5000, 
               fill='salmon', color='transparent',
               dotsize=0.3) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x='Cargos Medicos(USD)',
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

insurance_df %>%
  ggplot(aes(x=charges)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_dotplot(binwidth=2000, 
               fill='salmon', color='transparent',
               dotsize=0.3) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x='Cargos Medicos(USD)',
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))


insurance_male_smoker %>%
  ggplot(aes(x=charges)) +
  geom_histogram(binwidth = 4000,color='transparent',fill='darkgreen') +
  stat_bin(aes(y=..count..,label=..count..),binwidth = 4000,geom = 'text',vjust=-1)+
  labs(x='Cargos Medicos(USD)',y='Cantidad de asegurados',
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))


insurance_df %>%
  ggplot(aes(x=charges))+
  geom_histogram(binwidth = 2000,color='transparent',fill='darkblue')+
  stat_bin(aes(y=..count..,label=..count..),binwidth = 2000,geom = 'text',vjust=-1)+
  labs(x='Cargos Medicos(USD)',y='Cantidad de asegurados',
       title='Costo del seguro de salud')+
  theme_minimal()


insurance_df %>%
  filter(smoker=='no')%>%
    ggplot(aes(x=charges))+
    geom_histogram(binwidth = 2000,color='transparent',fill='darkblue')+
    stat_bin(aes(y=..count..,label=..count..),binwidth = 2000,geom = 'text',vjust=-1)+
    labs(x='Cargos Medicos(USD)',y='Cantidad de asegurados',
         title='Costo del seguro de salud')+
    theme_minimal()

g <- ggplot(insurance_df) + stat_bin(aes(x=charges))
a <- ggplot_build(g)
datos <- a[1]$data[[1]]
datos$count


insurance_df %>%
  filter(sex=='male')%>%
  ggplot(aes(x=charges,y=..density..,fill = smoker))+
  geom_histogram(binwidth = 2000,position = 'identity',alpha=0.5)+
  geom_density(bw=5000,position='identity',alpha=0.5)+
  labs(y='Cuentas', x='Costo del seguro [USD]')


insurance_df %>%
  filter(sex=='female')%>%
  ggplot(aes(x=charges,fill = smoker))+
  geom_density(bw=3000,position = 'identity',alpha=0.4)

insurance_df %>%
  ggplot(aes(x=charges))+
  geom_freqpoly(aes(y=..density..,color=region),binwidth=5000)

# Gráfico de densidad con ridgelines por región
insurance_df %>%
  ggplot(aes(x = charges,y=region, fill = region)) +
  geom_density_ridges(alpha = 0.5,bandwidth=4000) +
  theme_minimal() +
  labs(title = "Distribución de costos de seguro por región",
       x = "Costos del seguro",
       y = "Región")


insurance_df %>%
  ggplot(aes(x=charges,y=region,fill=region))+
  geom_violin()+
  geom_jitter(width = 0.1,alpha=0.3)+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()