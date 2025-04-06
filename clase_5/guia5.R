library(tidyverse)

df_players <- read_csv('playermatches.csv')
df_shots <- read_csv('shots.csv')

df_players$yellow_per_game <- df_players$yellowCard / df_players$games

# 3
df_players %>%
  group_by(position_general)%>%
  summarise(amarillas_prom = mean(yellowCard))


# 4
median_games <- df_players %>%
summarise(prom_partidos = median(games,na.rm=T))

df_players %>%filter(games >= 32)%>%
  group_by(position_general)%>%
  summarise(amarillas_prom = mean(yellowCard))


# 5
df_players %>% slice_max(yellowCard,n=10)%>%
  summarise(player=player,yellow_card = yellowCard)

# 6
df_players %>% slice_max(yellow_per_game,n=10)%>%
  summarise(player=player,yellow_card = yellowCard)

#9
df_players %>% group_by(position_general)%>%
  summarise(goles = sum(goals),tasa_conversion=sum(goals)/sum(shots))%>%
  arrange(desc(goles))


#10

df_players %>%
  filter(position_general=='Delantero')%>%
  ggplot(aes(x=goals,y=assists))+
  geom_point(alpha=0.2)+
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1)+
  ylim(c(0,200))


#11
df_shots %>% filter(player=='Lionel Messi' & shotResult=="Goal")%>%
  summarise(minutos_aprox = median(minute))

df_shots %>% filter(player=='Lionel Messi' & shotResult=="Goal")%>%
  ggplot(aes(x=minute))+
  geom_histogram(binwidth = 5)
  #scale_x_continuous(breaks = seq(0,100,by=5))

df_shots %>% filter(player =='Lionel Messi' & shotType %in% c('LeftFoot','RightFoot'))%>%
  ggplot(aes(x=player))+
  geom_bar(aes(fill=shotType),position = 'fill')

players_3 <- c('Lionel Messi','Luis Suárez','Neymar')

df_shots %>% filter(player %in% players_3 & shotResult=='Goal')%>%
  ggplot(aes(x=minute,fill = player))+
  geom_density(alpha=0.4)+
  theme_minimal()


