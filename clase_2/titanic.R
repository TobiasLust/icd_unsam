library(tidyverse)

titanic_df <- read_csv("titanic.csv",locale = locale(decimal_mark = ",")) 

## MIN Y MAX

min(titanic_df$age, na.rm = T) # nos devuelve solo un valor

titanic_df %>%
  slice_min(age) %>%
  relocate(survived)

titanic_df %>%
  slice_max(age,n=5) %>%
  relocate(survived) #los slice me devuelven la observacion

#el boleto mas barato
titanic_df %>%
  slice_min(fare) # 4.01

#el boleto mas caro
titanic_df %>%
  slice_max(fare)%>%
  relocate(fare) #512

unique(titanic_df$Port_of_Embarkation)
unique(titanic_df$class)

## ORDENAR

titanic_df %>%
  arrange(fare)%>%
  relocate(Port_of_Embarkation,fare)%>%
  head(1)

titanic_df %>%
  arrange(desc(fare))%>%
  relocate(Port_of_Embarkation,fare)%>%
  head(1)

titanic_df %>%
  arrange(age)%>%
  relocate(Port_of_Embarkation)%>%
  head(2) #los dos pasajeros mas jovenes salieron de C y S

titanic_df %>%
  arrange(desc(age))%>%
  relocate(class)%>%
  head(2) # los dos pasajeros mas longevos eran de 1 y 3 clase


titanic_df %>%
  arrange(fare,age)

titanic_df %>%
  arrange(age,fare)


## FILTRAR

titanic_first <- titanic_df %>%
  filter(class=='First')

titanic_age <- titanic_df$age

titanic_df %>%
  filter(sex=="female") %>%
  slice_max(age) ## la edad maxima de las mujeres es de 63

titanic_df %>%
  filter(who=="child") %>%
  slice_max(age)

titanic_df %>%
  filter(who!="child") %>%
  slice_min(age) ## la edad maximo para niños es de 15

## GRAFICO DE BARRAS

titanic_df %>%
  ggplot(aes(x=sex))+
  geom_bar()

titanic_df %>%
  ggplot(aes(x=siblings_of_the_passenger))+
  geom_bar()

titanic_df %>%
  ggplot(aes(x=who))+
  geom_bar()

titanic_df %>%
  ggplot(aes(x=sex))+
  geom_bar(aes(fill = class))
#Rename variable
titanic_df <- rename(titanic_df, 'Port' = 'Puerto')

titanic_df %>%
  ggplot(aes(x=who))+
  geom_bar(aes(fill=class))

titanic_df %>%
  ggplot(aes(x=class))+
  geom_bar(aes(fill=who))

titanic_df %>%
  ggplot(aes(x=alone))+
  geom_bar(aes(fill=who))

titanic_df %>%
  ggplot(aes(x=alone))+
  geom_bar(aes(fill=class))



titanic_df %>%
  ggplot(aes(x=alone))+
  geom_bar(aes(fill=class),position='dodge')

titanic_df %>%
  ggplot(aes(x=alone))+
  geom_bar(aes(fill=who),position='fill')+
  labs(x='Viaja Solo',y='Proporcion')


titanic_df %>%
  filter(Port=="S")%>%
  ggplot(aes(x=Port))+
  geom_bar(aes(fill=sex),position='fill')+
  labs(title = 'Proporcion de personas que embarcaron en Southp',x='Puerto',y='Proporcion') #de S el %25 aprox eran mujeres


titanic_df %>%
  ggplot(aes(x=Port))+
  geom_bar(aes(fill=class),position='fill') # la mayor proporcion de 3 clase salio de Q


titanic_df %>%
  filter(Port=='Q')%>%
  ggplot()+
  geom_bar(aes(x=Port,fill=class))


titanic_df %>%
  filter(Port=='S')%>%
  ggplot()+
  geom_bar(aes(x=Port,fill=survived),position = 'dodge')

titanic_df %>%
  filter(sex=='female')%>%
  ggplot()+
  geom_bar(aes(x=fct_rev(fct_infreq(Port))))+
  labs(title = 'Cantidad de Mujeres que embarcaron por puerto',x='Puerto de embarque',y='Cantidad de mujeres')

titanic_df %>%
  ggplot()+
  geom_bar(aes(x=Port,fill = sex),position='fill')+
  labs(title = 'Proporcion de Mujeres que embarcaron por puerto',x='Puerto de embarque',y='Proporcion')+
  theme_minimal()

titanic_df %>%
  ggplot(aes(x = Port, fill = sex)) +
  geom_bar(position = "fill") +
  labs(
    title = "Fracción de pasajeras mujeres en cada puerto",
    x = "Puerto de embarque",
    y = "Fracción"
  ) +
  theme_minimal()

titanic_df%>%
ggplot() + geom_bar(aes(x=survived, fill=class), position = 'fill') +
  labs(x ="¿Sobrevivió?", y = "Proporción") +
  scale_x_discrete(labels=c('No', 'Sí')) +
  scale_fill_manual(name = "Clase", labels = c("Primera", "Segunda", "Tercera"),
                    values = c("tomato3", "darkseagreen3", "darkorchid"))+
  theme_minimal()


titanic_df %>%
  ggplot(aes(x = Port, fill = sex)) +
  geom_bar(position = "fill") +
  stat_count(geom = 'text',aes(label=stat(count)),position = position_fill(vjust=0.5),color='black')+
  labs(
    title = "Fracción de pasajeras mujeres en cada puerto",
    x = "Puerto de embarque",
    y = "Fracción"
  ) +
  theme_minimal()

## AGRUPAR

titanic_who <- titanic_df %>% group_by(who)
titanic_who %>% summarise(observaciones = n())
titanic_who %>% summarise(edad_promedio = mean(age,na.rm=T))
titanic_df %>% summarise(edad_promedio = mean(age,na.rm=T))

titanic_class <- titanic_df %>%
  group_by(class)
titanic_class %>% summarise(cantidad_pasajeros = n())

titanic_survived <- titanic_df %>%
  group_by(survived)
titanic_survived %>% summarise(cantidad_pasajeros = n())


titanic_siblings <- titanic_df %>%
  group_by(siblings_of_the_passenger)
titanic_siblings %>% summarise(cantidad_pasajeros = n())


titanic_class %>%
  summarise(edad_prom = mean(age,na.rm=T))%>%
  ggplot(aes(x=class,y=edad_prom))+
  geom_col()+
  theme_minimal()

titanic_df %>%
  group_by(Port)%>%
  summarise(fare_prom = mean(fare,na.rm=T))%>%
  ggplot(aes(x = Port, y = fare_prom, fill = Port)) +
  geom_col() +
  labs(
    title = "Precio promedio del boleto por puerto de embarque",
    x = "Puerto de embarque",
    y = "Tarifa promedio"
  ) +
  theme_minimal()


titanic_survived %>%
  summarise(edad_prom = mean(age,na.rm=T))


titanic_class %>%
  summarise(boleto_prom = median(fare,na.rm=T))


titanic_class %>%
  ggplot(aes(x=class,y=fare))+
  geom_jitter()



