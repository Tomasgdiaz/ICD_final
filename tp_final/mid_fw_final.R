library(tidyverse)
library(modelr)
library(lubridate)
options(na.action = na.warn)

premier = read.csv("/home/infolab/Descargas/archive/EPL_20_21.csv")

#||||MEDIOCAMPISTAS||||

#filtramos por mediocampistas mayores a 25 y menores que 30

pmid = filter(premier, Position == "MF"|Position=="DF,MF"|Position=="MF,DF")
pmid = filter(pmid, Age>25 & Age<30) 

#agregamos la variable de pases cokçmpletados, multiplicando el porcentaje por los pases intentados
pmid1 = mutate(pmid, pases_completados = (Passes_Attempted*Perc_Passes_Completed) /100)

#graficamos los pases intentados por los pases completados
ggplot(pmid1, aes(Passes_Attempted,pases_completados)) + geom_point()

#agregamos el mod con las predicciones 
mod = lm(pases_completados~Passes_Attempted, data = pmid1)

grid <- pmid1 %>%
  data_grid(Passes_Attempted)

grid <- grid %>%
  add_predictions(mod)

#graficamos la regresion lineal
ggplot(pmid1, aes(x=Passes_Attempted, y=pases_completados)) + geom_point() + geom_line(data=grid, aes(y=pred), colour = "red", size = 1)

#agregamos los residuos
pmid1 = pmid1%>%
  add_residuals(mod)
#filtramos por residuos positivos
pmid1 = pmid1 %>% 
  filter(resid>0)
#graficamos los jugadores con residuos positivos
ggplot(pmid1, aes(Passes_Attempted, resid)) +
  geom_ref_line(h = 0) +
  geom_point()+ geom_text(aes(label=ifelse(pmid1$resid>95,as.character(Name),'')),hjust=0,vjust=-0.5) + labs(x='Pases intentados',
                                                                                                             y='Residuos',
                                                                                                             title='Residuos de pases intentados vs pases completos')

#||||DELANTEROS||||

#filtramos por delanteros 
delanteros =filter(premier, Position== "FW") %>% 
  filter(Matches>5)

#graficamos los delanteros por goles y partidos
ggplot(delanteros, aes(Matches, Goals)) + geom_point() 

#agregamos mod y predicciones 
mod1 = lm(Goals~Matches, data = delanteros)

delanteros1 = delanteros %>% 
  add_predictions(mod1)

#graficamos la regresion lineal
ggplot(delanteros, aes(x=Matches, y=Goals)) + geom_point() + geom_line(data=delanteros1, aes(y=pred), colour = "red", size = 1)

#como los que tienen mas goles los q tienen mas de 30 partidos
delanteros1 = delanteros %>% 
  filter(Matches>30)

#agregamos el nuevo mod con las predicciones con los delanteros mayores a 30
mod2 = lm(Goals~Matches, data = delanteros1)

delanteros1 = delanteros1 %>% 
  add_predictions(mod2)

#graficamos la regresion lineal con los delanteros mayores a 30
ggplot(delanteros1, aes(x=Matches, y=Goals)) + geom_point() + geom_line(data=delanteros1, aes(y=pred), colour = "red", size = 1) + geom_text(aes(label=ifelse(delanteros1$Goals>20,as.character(Name),'')),hjust=0.5,vjust=-0.5)

#agregamos los residuos
delanteros1 = delanteros1%>%
  add_residuals(mod2)
delanteros1 = delanteros1 %>% 
  filter(resid>0)

#graficamos los residuos de los delanteros
ggplot(delanteros1, aes(Matches, resid)) +
  geom_ref_line(h = 0) +
  geom_point()+ geom_text(aes(label=ifelse(delanteros1$resid>9,as.character(Name),'')),hjust=0,vjust=-0.5) 


#||||ARQUEROS||||

#Filtramos por arqueros
arq = filter(premier, Position=="GK")
