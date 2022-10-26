library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(ggridges)
library(modelr)
premier <- read.csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/EPL_20_21.csv')
fifa <- read_csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/players_20.csv')

pmid = filter(premier, Position == "MF"|Position =="MF,FW"|Position=="FW,MF"|Position=="DF,MF"|Position=="MF,DF")
pmid = filter(pmid, Age>25 & Age<30) 
 # filter(Perc_Passes_Completed>90)

#l plot de número de pases en función del número de intentos,

mod1 <- lm(Perc_Passes_Completed~Passes_Attempted, pmid)

grid <- data_grid(pmid,Passes_Attempted)

grid <- add_predictions(pmid, mod1)

#Grafico de los mejores 6 y modelo lineal, DESCOMENTAR LINEA 12
plot <- ggplot(data=pmid) + 
  geom_col(aes(x=as.factor(Passes_Attempted), y=Perc_Passes_Completed,fill=Name)) + 
  labs(title="Modelo lineal ajustado de Edad vs Costo de seguro") + 
  xlab("Edad") + ylab("Costo de seguro(US$)") + 
  geom_line(data=grid,aes(x=as.factor(Passes_Attempted),y=pred,group=1),size=1.5)
plot
#Grafico de residuos
ggplot(data =pmid, aes(x = as.factor(Passes_Attempted), 
                         y = mod1$residuals)) +
  geom_point(aes(color = mod1$residuals)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) + theme(axis.text.x=element_text(angle=90,vjust=.2,hjust=1))
