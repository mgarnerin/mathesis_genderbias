setwd("/home/getalp/garnerim/Bureau/tv_debates")

library(readr)
library(ggplot2)
library(dplyr)

turns<- read_delim("turns_tv_debates.csv",";", 
                  escape_double = FALSE, 
                  col_types = cols(gender = col_factor(levels = c("male", "female")),
                                  id_speaker = col_character()), 
                  trim_ws = TRUE)

spk <- read_delim("turns_per_spk.csv", ";", 
                   escape_double = FALSE, 
                   col_types = cols(gender = col_factor(levels = c("male","female")),
                                    id_speaker = col_character()),
                   trim_ws = TRUE)

# Création de deux subset
gspk<-subset(spk,!is.na(spk$gender))  #on retire les locuteurs non genrés
gspk600<-subset(gspk,gspk$nb_turn<600) #on retire les locuteurs parlant plus de 600 fois
# ce dernier subset n'a été créé que pour mieux visualiser les données, pas pertinent en soi

# Histogrammes

ggplot(gspk,aes(x=gspk$nb_turn,fill=gspk$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 10)+
  labs(title="Turn count per speaker in TV debates",x="Number of turns",y="Frequency",fill="Gender")

ggplot(gspk,aes(x=gspk$nb_turn,fill=gspk$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 5)+
  labs(title="Turn count per speaker in TV debates",x="Number of turns",y="Frequency",fill="Gender")+
  coord_cartesian(xlim=c(0,300))

# Boxplots

ggplot(gspk,aes(x='',y=gspk$nb_turn, fill=gspk$gender))+geom_boxplot()+
  labs(title="Distribution of the number of turn per speaker by gender in TV debates",x="",y="Turn count per speaker",fill="Gender")

# Affichage des locuteurs parlant moins de 600 fois

ggplot(gspk600,aes(x='',y=gspk600$nb_turn, fill=gspk600$gender))+geom_boxplot()+
  labs(title="Distribution of the number of turn per speaker by gender in TV debates",x="",y="Turn count per speaker",fill="Gender")


# == TIRAGE ALEATOIRE POUR EQUILIBRER LES ECHANTILLONS ======================

females<-subset(gspk,gender=="female")
males<-subset(gspk,gender=="male")

tir1<-sample_n(males,57)
tir2<-sample_n(males,57)
tir3<-sample_n(males,57)
tir4<-sample_n(males,57)
tir5<-sample_n(males,57)
tir6<-sample_n(males,57)
tir7<-sample_n(males,57)
tir8<-sample_n(males,57)
tir9<-sample_n(males,57)
tir10<-sample_n(males,57)

subset1<-rbind(females,tir1)
subset2<-rbind(females,tir2)
subset3<-rbind(females,tir3)
subset4<-rbind(females,tir4)
subset5<-rbind(females,tir5)
subset6<-rbind(females,tir6)
subset7<-rbind(females,tir7)
subset8<-rbind(females,tir8)
subset9<-rbind(females,tir9)
subset10<-rbind(females,tir10)

# Echelle log

ggplot(subset1,aes(x="",y=subset1$nb_turn,fill=subset1$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 1",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset2,aes(x="",y=subset2$nb_turn,fill=subset2$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 2",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset3,aes(x="",y=subset3$nb_turn,fill=subset3$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 3",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset4,aes(x="",y=subset4$nb_turn,fill=subset4$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 4",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset5,aes(x="",y=subset5$nb_turn,fill=subset5$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 5",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset6,aes(x="",y=subset6$nb_turn,fill=subset6$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 6",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset7,aes(x="",y=subset7$nb_turn,fill=subset7$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 7",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset8,aes(x="",y=subset8$nb_turn,fill=subset8$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 8",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset9,aes(x="",y=subset9$nb_turn,fill=subset9$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 9",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

ggplot(subset10,aes(x="",y=subset10$nb_turn,fill=subset10$gender))+
  geom_boxplot()+
  labs(title="Turn count per speaker in TV debates 10",x="",y="Number of turns",y="Frequency",fill="Gender")+
  scale_y_log10()

# == TESTS : REGRESSION DE POISSON (pour chaque tirage) =================================================================


fit<-glm(gspk$nb_turn~gspk$gender,family=poisson())
summary(fit)


fit1<-glm(subset1$nb_turn~subset1$gender,family=poisson())
summary(fit1)

fit2<-glm(subset2$nb_turn~subset2$gender,family=poisson())
summary(fit2)

fit3<-glm(subset3$nb_turn~subset3$gender,family=poisson())
summary(fit3)

fit4<-glm(subset4$nb_turn~subset4$gender,family=poisson())
summary(fit4)

fit5<-glm(subset5$nb_turn~subset5$gender,family=poisson())
summary(fit5)

fit6<-glm(subset6$nb_turn~subset6$gender,family=poisson())
summary(fit6)

fit7<-glm(subset7$nb_turn~subset7$gender,family=poisson())
summary(fit7)

fit8<-glm(subset8$nb_turn~subset8$gender,family=poisson())
summary(fit8)

fit9<-glm(subset9$nb_turn~subset9$gender,family=poisson())
summary(fit9)

fit10<-glm(subset10$nb_turn~subset10$gender,family=poisson())
summary(fit10)
