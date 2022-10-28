library(tidyverse)
library(modelr)
library(lubridate)
options(na.action = na.warn)

premier = read.csv("C:/Users/RYZEN/Desktop/ICD/EPL_20_21.csv")
pmid = filter(premier, Position == "MF"|Position =="MF,FW"|Position=="FW,MF"|Position=="DF,MF"|Position=="MF,DF")
pmid = filter(pmid, Age>25 & Age<30)

pmid1 = mutate(pmid, pases_completados = (Passes_Attempted*Perc_Passes_Completed) /100)


ggplot(pmid1, aes(Passes_Attempted,pases_completados)) + geom_point()

mod = lm(pases_completados~Passes_Attempted, data = pmid1)

grid <- pmid1 %>%
  data_grid(Passes_Attempted)

grid <- grid %>%
  add_predictions(mod)

ggplot(pmid1, aes(x=Passes_Attempted, y=pases_completados)) + geom_point() + geom_line(data=grid, aes(y=pred), colour = "red", size = 1)

pmid1 = pmid1%>%
  add_residuals(mod)
pmid1 = pmid1 %>% 
  filter(resid>0)

ggplot(pmid1, aes(Passes_Attempted, resid)) +
  geom_ref_line(h = 0) +
  geom_point()+ geom_text(aes(label=ifelse(pmid1$resid>50,as.character(Name),'')),hjust=0,vjust=0.5)
