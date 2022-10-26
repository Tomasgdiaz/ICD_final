library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(nycflights13)
library(ggridges)
library(lubridate)
premier <- read.csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/EPL_20_21.csv')
fifa <- read_csv('C:/Users/master/Desktop/UNSAM/ICD/tp_final/data/players_20.csv')
#elimino columnas
fifa [, c (1,2,6,11,12,14,17,18,19,20,21,22,23,24,27,28,29,30,31,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,59,60,61,63,64,65,66,67,68,69,70,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104)] <- list (NULL)
premier1 <- separate(premier, Name, into = c('nombre', 'apellido'), sep = "\\s")

fifa1 <- separate(fifa, short_name, into = c('nombre', 'apellido'), sep = "\\s")
#uno datasets, pierdo 60 filas pero no pude hacer que se queden
completo <- left_join(premier1,fifa1, by='apellido') %>% 
  group_by(apellido) %>% 
  filter(!duplicated(apellido))

#Saco valor total de los jugadores para buscar un promedio
#Al parecer no sirve, por las dudas lo dejo por si necesitamos
completo$total <-  sum(completo$value_eur, na.rm=TRUE)
#Dejo solo las columnas de EPL y value_eur
completo[, c (20,22,23,24,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)] <- list(NULL)
completo[, c (21,22)] <- list(NULL)
