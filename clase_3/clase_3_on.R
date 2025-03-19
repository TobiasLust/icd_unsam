library(tidyverse)

df <- read_csv('titanic.csv')

#Según frecuencia (fct_infreq)

#Para ordenar según cantidades/frecuencia de aparición

ggplot(data = df) +
  geom_bar(mapping = aes(x=fct_infreq(who), fill=alone))+
  ylab("Cantidad de personas")+
  xlab("Who")+
  scale_fill_discrete(name = "Viajó Sólo/a?", labels=c("NO","SI"))

#factor 
x1 <- c("Dic", "Abr", "Ene", "Mar")
x2 <- c("Dic", "Abr", "Eme", "Mar")

niveles_meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                   "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

y1 <- factor(x1,levels=niveles_meses)

library(datos)

encuesta_df <- encuesta

encuesta_interes <- summarise(group_by(encuesta_df,religion),
                              edad_prom = mean(edad,na.rm=T),
                              horas_tv_prom = mean(horas_tv,na.rm=T),
                              cantidad=n())
glimpse(encuesta_df)
levels(encuesta_df$religion)

ggplot(encuesta_interes) + geom_point(aes(x=horas_tv_prom,y=religion))

ggplot(encuesta_interes) + geom_point(aes(x=horas_tv_prom,y=fct_reorder(religion,horas_tv_prom),size=cantidad,color=edad_prom))


