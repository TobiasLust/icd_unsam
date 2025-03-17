library(tidyverse)

titanic_df <- read_csv('titanic.csv', locale= locale(decimal_mark=','))

# nos da informacion del type de las variables
spec(titanic_df)
# nos da informacion de cuantas observaciones (889) y variables(10) hay
glimpse(titanic_df)

min(titanic_df$age, na.rm = T)

slice_min(titanic_df,age, n=5) %>% relocate(survived)

slice_min(titanic_df, fare) %>% relocate(fare) #el boleto mas barato costo 4.01
slice_max(titanic_df, fare) %>% relocate(fare) # el mas caro 512

unique(titanic_df$class) # hay tres categorias de clase

arrange(titanic_df,age) #la mas joven salio de C y la segunda mas joven desde S
arrange(titanic_df,desc(age))#la primera mas longeva es de first y la segunda de third

arrange(titanic_df, desc(fare))


titanic_first <- filter(titanic_df, class=='First')
titanic_fem <- filter(titanic_df, sex=='female' )
slice_max(titanic_fem,age) # eedad max mujeres 63

titanic_child <- filter(titanic_df, who=='child')
titanic_adults <- filter(titanic_df, who!='child')
max(titanic_child$age) #edad maxima chicos 15
min(titanic_child$age) #edad minima 0.42
max(titanic_adults$age, na.rm = T)
min(titanic_adults$age, na.rm = T) # la edad minima para adultos es de 16

ggplot(titanic_df) +
  geom_bar(aes(x=sex))


ggplot(titanic_df) +
  geom_bar(aes(x=siblings_of_the_passenger))

ggplot(titanic_df) +
  geom_bar(aes(x=who))

ggplot(titanic_df) +
  geom_bar(aes(x=sex, fill= class)) # desagregar las barras con fill

ggplot(titanic_df) +
  geom_bar(aes(x=who,fill=class))

ggplot(titanic_df) +
  geom_bar(aes(x=class,fill=who))

ggplot(titanic_df) +
  geom_bar(aes(x=alone, fill= who))

ggplot(titanic_df) +
  geom_bar(aes(x=alone,fill = class), position = 'dodge')

ggplot(titanic_df) +
  geom_bar(aes(x=alone, fill= who),position='fill')+
  xlab('Viaja solo')+
  ylab('Proporcion')

ggplot(filter(titanic_df, Port_of_Embarkation == 'S'))+
         geom_bar(aes(x=Port_of_Embarkation, fill=who),position='fill')+
  xlab('Puerto')+
  ylab('Fraccion')


ggplot(filter(titanic_df, class == 'Third'))+
  geom_bar(aes(x=class, fill=Port_of_Embarkation),position='fill')+
  xlab('Clase')+
  ylab('Fraccion')

ggplot(filter(titanic_df, Port_of_Embarkation == 'Q'))+
  geom_bar(aes(x=Port_of_Embarkation, fill=class),position='fill')+
  xlab('Puerto')+
  ylab('Fraccion')

ggplot(filter(titanic_df, Port_of_Embarkation == 'S'))+
  geom_bar(aes(x=Port_of_Embarkation, fill=survived),position='dodge')+
  xlab('Puerto')+
  ylab('Cantidad')

ggplot(filter(titanic_df, sex == 'female'))+
  geom_bar(aes(x=sex, fill=Port_of_Embarkation),position='dodge')+
  xlab('Mujeres')+
  ylab('Cant')

ggplot(filter(titanic_df, sex == 'female'))+
  geom_bar(aes(x=sex, fill=Port_of_Embarkation),position='fill')+
  xlab('Mujeres')+
  ylab('Proporcion')


ggplot(titanic_df)+
  geom_bar(aes(x=Port_of_Embarkation, fill=sex),position='fill')+
  xlab('Puerto')+
  ylab('Proporcion')











