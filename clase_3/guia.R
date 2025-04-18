# El código en este archivo está pensando como guía para el trabajo de laboratorio. El código tal cuál está escrito no funciona, tienen que completar donde aparecen signos de interrogación.

library(tidyverse)

# Leemos los datos
df <- read_csv('/Users/mariasolespain/Downloads/insurance.csv')

# Punto 5 de la guía
df %>% 
  # Filtrar solo fumadores varones
  filter(smoker==???, ???) %>%
  ggplot(aes(x=???)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_dotplot(binwidth=???, 
               fill='salmon', color='transparent',
               dotsize=0.7) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x=???,
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

# Punto 8
???
  
  ## Ejemplo de como poner texto con cantidades
  bw <- ???
  df %>% 
  filter(smoker=='yes', sex=='male') %>%
  ggplot(aes(x=???)) +
  # Grafica el histograma
  geom_histogram(binwidth=bw, 
                 fill='darkgreen', color='transparent') +
  stat_bin(aes(y=..count.., label=..count..), binwidth=bw,
           geom='text', vjust=-.5) +
  labs(y=???, x='Costo del seguro [USD]')

# Punto 12
bw <- ???
  df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=???, fill=???)) +
  # Completen la posición. Vean la diferencia entre 'stack' y 'identity'
  geom_histogram(binwidth=bw, color='transparent', position='identity',
                 alpha=0.5) +
  labs(y='Cuentas', x='Costo del seguro [USD]')

# Punto 15
bw <- ???
  df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=???, fill=???)) +
  # Completen el mapeo usando la densidad ('density').
  geom_histogram(aes(y=???), position='identity', binwidth=bw, 
                 color='transparent', alpha=0.5) +
  labs(y='Cuentas', x='Costo del seguro [USD]')

# Punto 15
??? + geom_density()

# geom_freqpoly
bw <- 2000
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=???, fill=???)) +
  # Después de probar, cambiar esta línea para usar freqpoly
  geom_histogram(aes(y=..density..), position='identity', binwidth=bw, 
                 color='transparent', alpha=0.5) +
  labs(y='Cuentas', x='Costo del seguro [USD]')

# ggridges
install.packages('ggridges')
library(ggridges)
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=smoker, height = stat(density))) +
  # Prueben sacar "stat='density'". Cómo cambia el gráfico?
  geom_density_ridges(aes(y=region), alpha=0.5, stat='density') +
  labs(y='Region', x='Costo del seguro [USD]')

# geom_density2d
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, y=bmi, color=smoker)) +
  geom_density_2d() +
  scale_color_discrete(name='Fumador', labels=c('No', 'Sí')) +
  labs(y='Índice de masa corporal', x='Costo del seguro [USD]',
       title='Distribución de BMI y costos del seguro médico',
       subtitle='Varones de EEUU') +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 