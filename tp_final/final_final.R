library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggridges)
library(modelr)

premier <-read.csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/EPL_20_21.csv')#||||MEDIOCAMPISTAS||||

#Filtramos por mediocampistas mayores a 25 y menores que 30,
#Mediocampistas, mediocampistas defensivos

pmid = filter(premier, Position == "MF"|Position=="DF,MF"|Position=="MF,DF")
pmid = filter(pmid, Age>25 & Age<30) 

#Agregamos la variable de pases completados, multiplicando el porcentaje por los pases intentados
pmid1 = mutate(pmid, pases_completados = (Passes_Attempted*Perc_Passes_Completed) /100)

#Graficamos los pases intentados por los pases completados
ggplot(pmid1, aes(Passes_Attempted,pases_completados)) + geom_point()


#Agregamos el mod con las predicciones 
mod = lm(pases_completados~Passes_Attempted + I(Passes_Attempted^2), data = pmid1)

grid <- pmid1 %>%
  data_grid(Passes_Attempted)

grid <- grid %>%
  add_predictions(mod)

#Graficamos la regresion lineal
ggplot(pmid1, aes(x=Passes_Attempted, y=pases_completados)) + geom_point() + geom_line(data=grid, aes(y=pred), colour = "red", size = 1) + 
  labs(x='Pases intentados',
       y='Pases completos',
       title='Pases completos en función de los intentos')

#Agregamos los residuos
pmid1 = pmid1%>%
  add_residuals(mod)
#Filtramos por residuos positivos
pmid1 = pmid1 %>% 
  filter(resid>0)
#Graficamos los jugadores con residuos positivos
ggplot(pmid1, aes(Passes_Attempted, resid)) +
  geom_ref_line(h = 0) +
  geom_point()+ geom_text(aes(label=ifelse(pmid1$resid>95,as.character(Name),'')),hjust=0,vjust=-0.5) + labs(x='Pases intentados',
                                                                                                             y='Residuos',
                                                                                                             title='Residuos de pases intentados vs pases completos')
#||DELANTEROS||
#Filtramos por delanteros 
delanteros =filter(premier, Position== "FW") %>% 
  filter(Matches>5)
#SACO A KANE DEL LM
delanteros_k <- filter(delanteros,Goals < 20 )
mod_k <- lm(Goals~Matches, data = delanteros_k)


del_k <- delanteros_k %>% 
  add_predictions(mod_k)

ggplot(delanteros, aes(x=Matches, y=Goals)) + geom_point() + geom_line(data=del_k, aes(y=pred), colour = "red", size = 1) +
  labs(x='Partidos',
       y='Goles',
       title='Goles por partidos',
       subtitle= 'Modelo sin Harry Kane')

#Graficamos los delanteros por goles y partidos
ggplot(delanteros, aes(Matches, Goals)) + geom_point() 

#Agregamos mod y predicciones 
mod1 = lm(Goals~Matches, data = delanteros)

delanteros1 = delanteros %>% 
  add_predictions(mod1)

#Graficamos la regresion lineal
ggplot(delanteros, aes(x=Matches, y=Goals)) + geom_point() + geom_line(data=delanteros1, aes(y=pred), colour = "red", size = 1) +
  labs(x='Partidos',
       y='Goles',
       title='Goles por partidos',
       subtitle= 'Modelo con Harry Kane')

#Como los que tienen mas goles los que tienen mas de 30 partidos
delanteros1 = delanteros %>% 
  filter(Matches>30)

#Agregamos el nuevo mod con las predicciones con los delanteros mayores a 30
mod2 = lm(Goals~Matches, data = delanteros1)

delanteros1 = delanteros1 %>% 
  add_predictions(mod2)

#Graficamos la regresion lineal con los delanteros mayores a 30
ggplot(delanteros1, aes(x=Matches, y=Goals)) + geom_point() + geom_line(data=delanteros1, aes(y=pred), colour = "red", size = 1) + geom_text(aes(label=ifelse(delanteros1$Goals>20,as.character(Name),'')),hjust=0.5,vjust=-0.5) +
  labs(x='Partidos',
       y='Goles',
       title='Goles por partidos (zoom)',
       subtitle= 'Modelo teniendo en cuenta a Harry Kane')

#Agregamos los residuos
delanteros1 = delanteros1%>%
  add_residuals(mod2)
delanteros1 = delanteros1 %>% 
  filter(resid>0)

#Graficamos los residuos de los delanteros
ggplot(delanteros1, aes(Matches, resid)) +
  geom_ref_line(h = 0) +
  geom_point()+ geom_text(aes(label=ifelse(delanteros1$resid>9,as.character(Name),'')),hjust=0,vjust=-0.5) + labs(x='Partidos',
                                                                                                                y='Residuos',
                                                                                                                title='Residuos de goles por partidos')



#|| EXTREMOS ||
#|Tener en cuenta asistencias, pases, y goles
#|La media de pases intentados es de 703 aprox
pext <- filter(premier,Position=="FW,MF"|Position=="MF,FW"|Position == 'FW',
               Goals>5,Goals<23, Assists>5)
pext <- arrange(pext, desc(Assists)) %>% 
  head(n=5)

ggplot(pext, aes(Goals,Assists, fill=Name)) +      
  geom_col( position='dodge') + scale_y_continuous(breaks=seq(0, 10, 2))+scale_x_continuous(breaks=seq(5, 19, 1))  + 
  geom_text(aes(label=ifelse(Assists>2,as.character(Name),'')),hjust=0.5,vjust=-1) + labs(
    x='Goles',
    y='Asistencias',
    title='Asistencias en función de goles') + scale_fill_discrete(name='Jugadores')
  
ggplot(extremos, aes(Matches,Assists) ) + geom_point() + geom_text(aes(label=ifelse(Assists>9,as.character(Name),'')),hjust=+0.5,vjust=+2)+xlim(25,40) + scale_y_continuous(breaks=seq(0, 10, 1)) + 
  labs(x='Partidos',
       y='Asistencias',
       title='Asistencias según cantidad de partidos')

  
#||DEFENSORES||
#
defensores=filter(premier, Position== "DF") %>%
  filter(Mins>3100, Perc_Passes_Completed>78, Yellow_Cards<10)

ggplot(defensores, aes(Perc_Passes_Completed, Mins,color=Yellow_Cards>5)) + geom_point() +
  geom_text(aes(label=ifelse(Yellow_Cards>0,as.character(Name),'')),hjust=1,vjust=-0.5) +
  geom_text(aes(label=ifelse(Yellow_Cards<=5,as.character(Yellow_Cards),'')),hjust=-0.5,vjust=-0.5) +
  labs(x='Porcentaje de pases completados',
       y='Minutos jugados',
       title='Porcentaje de pases completos por minutos') + scale_color_discrete('Tarjetas Amarillas', label= c('5 o menos','Más de 5')) +
  xlim(76,90)

#||||ARQUEROS||||

#Filtramos por arqueros
arqueros = filter(premier, Position=="GK")

#Agrego los datos de los goles en contra por equipos "datos sacados de la pagina oficial de la premier league"
gc =read.csv("C:/Users/RYZEN/Desktop/ICD/Libro1.csv", sep= ";")

#Filtro para limpiar a los arqueros con menos de un partido
arqueros = filter(arqueros, Matches > 1)

#Uno los datasets
arqueros1 = full_join(arqueros,gc)

#Graficamos partidos jugados vs goles en contra
ggplot(arqueros1, aes(Matches,GC) ) + geom_point() + geom_text(aes(label=ifelse(arqueros1$GC<35,as.character(Name),'')),hjust=0,vjust=-0.5) + labs(x='Partidos Jugados',
                                                                                                                                                   y='Goles Recibidos',
                                                                                                                                                   title='Partidos Jugados vs Goles en Contra')

