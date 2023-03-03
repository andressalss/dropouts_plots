library(ggplot2)
library(tidyr)
library(tidyverse)
library(esquisse)

dados <- read_delim("ALUNOS_2017.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
dados_transformados <- dados %>% 
  select(CO_ALUNO, TP_SITUACAO, TP_TURNO, TP_SEXO, TP_COR_RACA, IDADE) %>% 
  mutate(TP_TURNO=ifelse(TP_TURNO==1,"MORNING",
                         ifelse(TP_TURNO==3,"NIGHT",
                                ifelse(TP_TURNO==2,"AFTERNOON",
                                       ifelse(TP_TURNO==4,"FULL-TIME",
                                              ifelse(is.na(TP_TURNO)==TRUE,"NA","NA"))))),
         TP_SITUACAO= ifelse(TP_SITUACAO==4,"EVADED",
                             ifelse(TP_SITUACAO==5,"EVADED",      
                                    ifelse(TP_SITUACAO==6,"GRADUATED","CENSORSHIP"))),
         TP_COR_RACA=ifelse(TP_COR_RACA==0,"NAO DECLARADO",
                            ifelse(TP_COR_RACA==1,"BRANCA",
                                   ifelse(TP_COR_RACA==2,"PRETA",
                                          ifelse(TP_COR_RACA==3,"PARDA",
                                                 ifelse(TP_COR_RACA==4,"AMARELA","INDIGENA"))))),
         TP_SEXO=(ifelse(TP_SEXO==1,"FEMALE","MALE")))

ggplot(data = dados_transformados, aes(Status, Students)) +
  geom_col(aes(fill = Status))

attach(dados_transformados)

sumarizados <- dados_transformados %>% 
  group_by(TP_SEXO) %>% 
  summarise(Students = n_distinct(CO_ALUNO))

as_tibble(table(TP_SITUACAO))

g_turno <- ggplot(data = dados_transformados, aes(TP_TURNO)) +
  geom_bar(aes(fill = TP_SITUACAO), position = "fill") +
  theme_linedraw() 


library(ggplot2)

shift <- ggplot(dados_transformados) +
 aes(x = TP_TURNO, fill = TP_SITUACAO) +
 geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd")+
 labs(x = "SHIFT", y = "STUDENS PROPORTION", fill = NULL) +
 theme_minimal()

gender <- ggplot(dados_transformados) +
  aes(x = TP_SEXO, fill = TP_SITUACAO) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd")+
  labs(x = "GENDER", y = "STUDENS PROPORTION", fill = NULL) +
  theme_minimal()

age <- ggplot(dados_transformados) +
  aes(x = IDADE, fill = TP_SITUACAO) +
  geom_histogram(bins = 30L) +
  scale_fill_brewer(palette = "YlOrRd")+
  labs(x = "AGE", y = "STUDENS", fill = "SITUATION")
  theme_minimal()

situation <- ggplot(dados_transformados) +
    aes(x = TP_SITUACAO) +
    geom_bar(fill = brewer.pal(3,"YlOrRd")) +
    labs(x = NULL, y = "STUDENS")+
    theme_minimal()
