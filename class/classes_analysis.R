#Définition du dossier de travail
setwd("/home/getalp/garnerim/Bureau/clean_data")

library(readr)

#Import des données
speakers <- read_delim("turn_count_length.csv", ";", 
                       escape_double = FALSE, 
                       col_types = cols(gender = col_factor(levels = c("male", "female")), 
                                        id_speaker = col_character(),
                                        class = col_factor(levels = c("1","2","3","4"))), 
                       trim_ws = TRUE)

library(ggplot2)

ggplot(speakers,aes(x=total_length,y=turn_count))+
  geom_jitter()+
  coord_cartesian(xlim=c(0,10000),ylim=c(0,1000))+
  geom_hline(yintercept = 75)+
  geom_vline(xintercept = 600)+
  labs(x="Longueur totale des interventions",y="Nombre de tours")

library(dplyr)

count(speakers,class=="1")
count(speakers,class=="2")
count(speakers,class=="3")
count(speakers,class=="4")

ggplot(speakers,aes(x=total_length,y=turn_count,color=gender))+
  geom_jitter()+
  coord_cartesian(xlim=c(0,10000),ylim=c(0,1000))+
  geom_hline(yintercept = 75)+
  geom_vline(xintercept = 600)+
  labs(x="Longueur totale des interventions",y="Nombre de tours")

ggplot(speakers,aes(x=,fill=gender))+
  geom_bar(position="dodge")+
  facet_wrap(~class)

library(scales)


ggplot(speakers, aes(x=gender,  group=class)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="", y = "Percent") +
  theme(legend.position="none")+
  facet_grid(~class) +
  scale_y_continuous(labels = scales::percent)


