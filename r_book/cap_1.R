library(tidyverse)
library(datos)

millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_point(aes(color=clase)) 
#stroke le agrega grosor
#color=cilindrada < 5 condicional , me muestra boleano


millas %>%
  ggplot(aes(x=cilindros,y=autopista))+
  geom_point()

# no me proporciona ningun tipoo de relacion
millas %>%
  ggplot(aes(x=traccion,y=clase))+
  geom_point()


#SEPARAR EN FACETAS
millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_point()+
  facet_wrap(~ clase,nrow=2)

#geom smooth

millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_smooth() 

millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_smooth(aes(linetype = traccion)) 


millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_smooth(aes(linetype = traccion,color=traccion))+
  geom_point(aes(color=traccion))

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))


millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_point(aes(color=clase))+
  geom_smooth()

millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_point(aes(color=clase))+
  geom_smooth(data = filter(millas,clase=="subcompacto"),se=FALSE)
  # para un subconjunto de datos, se saca la sombra de la dispersion

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista, color = traccion)) +
  geom_point() +
  geom_smooth(aes(linetype = traccion),se = FALSE)


millas %>%
  ggplot(aes(x=cilindrada,y=autopista))+
  geom_point(aes(color=traccion))+
  geom_smooth(aes(linetype = traccion),se=FALSE)



