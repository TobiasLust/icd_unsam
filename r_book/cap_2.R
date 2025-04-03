library(tidyverse)
library(datos)

diamantes %>%
  ggplot(aes(x=corte))+
  geom_bar()


# es lo mismo que geom bar porque geom bar usa stat= count
diamantes %>%
  ggplot(aes(x=corte))+
  stat_count()

# si no quiero que haga el conteo

demo <- tribble(
  ~corte,     ~freq,
  "Regular",   1610,
  "Bueno",     4906,
  "Muy Bueno", 12082,
  "Premium",   13791,
  "Ideal",     21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = corte, y = freq), stat = "identity")

diamantes %>%
  ggplot(aes(x=corte))+
  geom_bar(aes(fill=claridad),position = "dodge") #comparar valores individuales


diamantes %>%
  ggplot(aes(x=corte))+
  geom_bar(aes(fill=claridad),position = "fill") # proporciones

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista),position = "jitter")
#agrega ruido a los puntos si estan superpuestos


##ej

ggplot(data = millas, mapping = aes(x = ciudad, y = autopista)) +
  geom_jitter(width = 0.3, height = 0.3)
#le agrega ruido


ggplot(data = millas, mapping = aes(x = ciudad, y = autopista)) +
  geom_count() # te la cantidad de puntos

ggplot(data = millas, mapping = aes(x = clase, y = autopista)) +
  geom_boxplot()
