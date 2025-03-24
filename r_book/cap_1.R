library(tidyverse)
library(datos::millas)


# relacion entre tamaño del motor y la eficencia del combustible en autopista

#aes alpha para transparencia y size tamaño puntos , podes agregar logica a color
millas %>%
  ggplot() + geom_point(mapping = aes(x=cilindrada,y=autopista,color=clase ),position = "jitter")


millas %>%
  ggplot() + geom_smooth(mapping = aes(x=cilindrada,y=autopista,linetype = traccion ))


millas %>%
  ggplot(mapping = aes(x=cilindrada,y=autopista))+
  geom_point(aes(color=clase))+
  geom_smooth()


ggplot(data = millas, mapping = aes(x = ciudad, y = autopista)) +
  geom_point(position = "jitter")