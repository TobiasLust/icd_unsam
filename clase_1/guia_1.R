library(tidyverse)


# levantar dataset
data <- read_csv('properati_SM_SPA.csv')

# ver dataframe

view(data)

# info del dataset (tipos de datos,n_columnas,n_rows)

glimpse(data)

unique(data$l4)

# ver valores unicos de una columna 
distinct(select(data, l4))
distinct(select(data, tipo_propiedad))

count(data,l4)

# ver cuantas veces se repite el valor en la columna
count(data,tipo_propiedad) # casa 871 depto 1302

# ggplott argumentos : dataset, aes argumetos : columnas
# geom_point tipo de grafico o sea este caso de puntos
# lab nombre a los ejes
#geom_smooth para trazar una f lineal

ggplot(data,mapping=aes(x=sup_cubierta, y=precio, color=(factor(tipo_propiedad)))) +
geom_point() +
geom_smooth(method='lm')+
xlab("Superficie cubierta [m2]") +
ylab("Precio [USD]")











