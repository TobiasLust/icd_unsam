library(tidyverse)

ggplot(data,mapping = aes(x=promedio_trabajo_anual,y=nivel_felicidad, color=factor(continente), size= rank)) +
  geom_point() +
  ylim(5,8) +
  scale_color_discrete(name='Continente', labels=c('Africa','America','Asia','Europa','Oceania'))