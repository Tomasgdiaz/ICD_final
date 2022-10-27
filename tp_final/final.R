library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggridges)
library(modelr)
premier <- read.csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/EPL_20_21.csv')
#Usar para buscar los valores de cada uno
fifa <- read_csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/players_20.csv')
#Filtro por lo pedido por ESPN
pmid = filter(premier, Position == "MF"|Position =="MF,FW"|Position=="FW,MF"|Position=="DF,MF"|Position=="MF,DF")
pmid = filter(pmid, Age>25 & Age<30 & Passes_Attempted>500 & Assists > 1) 

mod1 <- lm(Perc_Passes_Completed~Passes_Attempted, pmid)

grid <- data_grid(pmid,Passes_Attempted)

grid <- add_predictions(pmid, mod1)


#l plot de número de pases en función del número de intentos,
plot <- ggplot(data=pmid) + 
  geom_point(aes(x=Passes_Attempted, y=Perc_Passes_Completed)) + 
  geom_line(data=grid,aes(x=Passes_Attempted,y=pred,group=1),size=1.5) +
  labs(x='Pases intentados',
       y='Pases completos(%)',
       title=' Porcentaje de pases completos en función de pases intentados') + 
  ylim(50,100)
plot

#Plot residuos y nombre de los mejores 5
ggplot(data =pmid, aes(x = as.factor(Passes_Attempted), 
                       y = mod1$residuals)) +
geom_point(aes(color = mod1$residuals)) +  
geom_text(aes(label=ifelse(mod1$residuals>4.5,as.character(Name),'')),hjust=0,vjust=0.5) + 
geom_line(size = 0.3) + theme(axis.text.x=element_text(angle=90,vjust=.2,hjust=1)) + 
labs(x='Pases intentados',
     y='Residuos',
     title='Residuos de pases intentados vs pases completos') + 
scale_color_gradient(guide = NULL)
