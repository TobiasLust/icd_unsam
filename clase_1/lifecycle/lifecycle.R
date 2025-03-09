ggplot(data,mapping = aes(x=costo_agua,y=expectativa_vida, color=factor(continente))) +
  geom_point()