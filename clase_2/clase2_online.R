library(tidyverse)
library(nycflights13)

# vuelo con mayor retraso, r base, te da solo el dato de la columna
max(flights$dep_delay,na.rm=TRUE)

#slice_max or min ordena de mayor a menos o al revez y te recorta la con el max o el min valor
slice_max(flights,dep_delay, n=10)


#filtra por var con nrow vemos la cantidad de rows
filter(flights,month==3,day==20)

time_win <- mutate(flights, time_win = dep_delay - arr_delay)
view(time_win)
glimpse(time_win)
slice_max(time_win, time_win,n=5)

summarise(flights, cantidad.de.vuelos=n(),suma.de.retrasos=sum(dep_delay, na.rm=T))

airports <- group_by(flights,origin)

summarise(airports,cant_vuelos =n())

count(flights,origin)

flights_jfk <- filter(flights,origin=='JFK')
count_flights <- count(flights_jfk,dest)

slice_max(count(flights_jfk,dest),n)

vuelos_meses <- group_by(flights,month)
conteo_vuelos <- summarise(vuelos_meses,cant_vuelos=n())
slice_max(conteo_vuelos,cant_vuelos)



