setwd("/home/getalp/garnerim/Bureau/analyse_Z_data/Train_ASR")

library(readr)
#install.packages("dplyr")
library(dplyr)
#install("ggplot2")
library("ggplot2")

#Import des données
speakers <- read_delim("text_turns_by_spk_gendered_with_class.csv", ";", 
                       escape_double = FALSE, 
                       col_types = cols(gender = col_factor(levels = c("male", "female")), 
                                        name = col_character(),
                                        spk_class = col_factor(levels = c("1","2","3","4"))), 
                       trim_ws = TRUE)

# Représentation des proportions des genre dans chaque classe médiatique

library(scales)
ggplot(speakers, aes(x=gender,  group=spk_class)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="", y = "Percent") +
  theme(legend.position="none")+
  facet_grid(~spk_class) +
  scale_y_continuous(labels = scales::percent)


# Sommes des tours de parole

summary(speakers)

sum(na.exclude(speakers$nb_turn[speakers$gender=="female"]))
sum(na.exclude(speakers$nb_turn[speakers$gender=="male"]))
sum(speakers$nb_turn[is.na(speakers$gender)],rm.na=T)
sum(speakers$nb_turn)


# HISTOGRAMMES : nombre de tours de parole par locuteur en fonction du genre

ggplot(speakers,aes(x=speakers$nb_turn,fill=speakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker",x="Number of turns",y="Frequency",fill="Gender")

ggplot(speakers,aes(x=speakers$nb_turn,fill=speakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker (x<500)",x="Number of turns",y="Frequency",fill="Gender")+
  coord_cartesian(xlim=c(0,500),ylim=c(0,150))

ggplot(speakers,aes(x=speakers$nb_turn,fill=speakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker (10<x<100)",x="Number of turns",y="Frequency",fill="Gender")+
  coord_cartesian(xlim=c(10,100),ylim=c(0,45))

# Sans les NA

gspeakers<-subset(speakers,!speakers$gender=="NA")

ggplot(gspeakers,aes(x=gspeakers$nb_turn,fill=gspeakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker",x="Number of turns",y="Frequency",fill="Gender")

ggplot(gspeakers,aes(x=gspeakers$nb_turn,fill=gspeakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker (x<500)",x="Number of turns",y="Frequency",fill="Gender")+
  coord_cartesian(xlim=c(0,500))

ggplot(gspeakers,aes(x=gspeakers$nb_turn,fill=gspeakers$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 1)+
  labs(title="Turn count per speaker (10<x<100)",x="Number of turns",y="Frequency",fill="Gender")+
  coord_cartesian(xlim=c(10,100),ylim=c(0,45))


# Boxplot représentant le nombre de tour de paroles par locuteur en fonction du genre 
# (-> répartition inégale dans les valeurs extrêmes)
ggplot(speakers,aes(x="",y=speakers$nb_turn,fill=speakers$gender))+
  geom_boxplot()+
  scale_x_discrete(name="")+
  scale_y_continuous(name='Turn count per speaker')+
  scale_fill_discrete(name="Gender")+
  ggtitle("Répartition du nombre de tours de parole en fonction du genre")

# Idem sur l'intervalle 0-100 pour plus de lisibilité
# -> On remarque que la médiane des femmes est légèrement plus haute que celle des hommes
# -> mais également que chez les femmes la distribution semble plus compacte que chez les hommes
ggplot(speakers,aes(x="",y=speakers$nb_turn,fill=speakers$gender))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,100))+
  scale_x_discrete(name="")+
  scale_y_continuous(name='Turn count per speaker')+
  scale_fill_discrete(name="Gender")+
  ggtitle("Répartition du nombre de tours de parole en fonction du genre (0-100)")


# Idem sur l'intervalle 60-1000 pour plus de lisibilité
# -> On remarque que chez les hommes plus de valeurs extrêmes -> 205 - 600 : doit correspondre à un rôle donné
ggplot(speakers,aes(x="",y=speakers$nb_turn,fill=speakers$gender))+
  geom_boxplot()+
  coord_cartesian(ylim=c(60,1000))+
  scale_x_discrete(name="")+
  scale_y_continuous(name='Turn count per speaker')+
  scale_fill_discrete(name="Gender")+
  ggtitle("Répartition du nombre de tours de parole en fonction du genre (60-1000)")

# ================================================================================================




# ================================================================================================

turns_z<- read_delim("text_turns_gendered_with_class.csv", ";", 
                   escape_double = FALSE, 
                   col_types = cols(gender = col_factor(levels = c("female", "male")), 
                                    id_turn = col_character(), 
                                    spk_class = col_factor(levels = c("1", "2", "3", "4"))), 
                   trim_ws = TRUE)


lengths<-turns$end_time-turns$start_time
genders<-turns$gender

glengths<-data.frame(lengths,genders)

mean(glengths$lengths[glengths$gender=="male"],na.rm=TRUE)
mean(glengths$lengths[glengths$gender=="female"],na.rm=TRUE)
mean(glengths$lengths[is.na(glengths$genders)],na.rm=TRUE)

sd(glengths$lengths[glengths$gender=="male"],na.rm=TRUE)
sd(glengths$lengths[glengths$gender=="female"],na.rm=TRUE)
sd(glengths$lengths[is.na(glengths$genders)],na.rm=TRUE)

ggplot(glengths,aes(x="",y=glengths$lengths,fill=glengths$gender))+
  geom_boxplot()+
  scale_y_continuous(name="")+
  scale_fill_discrete(name="Gender")+
  ggtitle("Turn length by gender")



