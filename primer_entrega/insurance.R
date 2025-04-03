library(tidyverse)

df <- read.csv('insurance.csv')


# Como aumentan los cargos del seguro en funcion de indice de masa corporal,
# diviendolos en fumador o no fumador.
ggplot(df)+geom_point(aes(x=bmi,y=charges,color=smoker))+
  labs(x='Indice Masa Corporal',y='Cargos Seguro Medico [USD]')

# Realizar un gráfico de dispersión entre las variables age y charges
ggplot(df)+geom_point(aes(x=age,y=charges,color=smoker))+
  labs(x='Edad del cliente', y='Cargos [USD]')+
  scale_color_discrete(name='Fumador',labels=c('No','Si'))

#Encontrar la persona que paga menos seguro medico
arrange(df,charges)

df_region <- group_by(df,region)

count(df_region)

df_hombres <- group_by(filter(df,sex=='male'),region)
count(df_hombres)

# cantidad de fumadores por región
ggplot(df)+
  geom_bar(aes(x=region, fill=smoker),position='dodge')+
  labs(x='Region',y='Cantidad de clientes',title = 'Cantidad de fumadores por region')+
  scale_fill_manual(name = "Fumador", labels = c("No", "Si"),
             values = c("blue", "red"))

ggplot(data = df) + geom_bar(aes(x=region, fill=smoker), position='fill')+
  labs(x='Region',y='Porcentaje clientes',title = 'Region mas afecta por el habito de fumar')+
  scale_fill_discrete(name='Fumador', labels=(c("No","Si")))+
  scale_y_continuous(labels = scales::percent)

