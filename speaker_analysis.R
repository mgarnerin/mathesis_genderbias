#Définition du dossier de travail
setwd("/home/getalp/garnerim/Bureau/scripts/turn_by_spk_csv")
#install.packages("scales")
library(scales)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(readr)

#Import des données
speakers <- read_delim("turns_wtht_zero.csv", ";", 
                    escape_double = FALSE, 
                    col_types = cols(gender = col_factor(levels = c("male", "female")), 
                                     id_speaker = col_character()), 
                    trim_ws = TRUE)


# = ANALYSE GLOBALE ====================================================================

# Locuteurs 

#Tous corpus confondus
count(speakers,gender=="male")
count(speakers,gender=="female")
count(speakers,is.na(gender))

#ESTER1
count(speakers,gender=="male"&speakers$ester1>0)
count(speakers,gender=="female"&speakers$ester1>0)
count(speakers,is.na(gender)&speakers$ester1>0)

#ESTER2
count(speakers,gender=="male"&speakers$ester2>0)
count(speakers,gender=="female"&speakers$ester2>0)
count(speakers,is.na(gender)&speakers$ester2>0)

#ESTER3
count(speakers,gender=="male"&speakers$etape>0)
count(speakers,gender=="female"&speakers$etape>0)
count(speakers,is.na(gender)&speakers$etape>0)

#ESTER4
count(speakers,gender=="male"&speakers$repere>0)
count(speakers,gender=="female"&speakers$repere>0)
count(speakers,is.na(gender)&speakers$repere>0)


#Tours de paroles

#Tous corpus confondus
sum(na.exclude(speakers$all[speakers$gender=="male"]))
sum(na.exclude(speakers$all[speakers$gender=="female"]))
sum(na.exclude(speakers$all[is.na(speakers$gender)]))

#ESTER1
sum(na.exclude(speakers$ester1[speakers$gender=="male"]))
sum(na.exclude(speakers$ester1[speakers$gender=="female"]))
sum(na.exclude(speakers$ester1[is.na(speakers$gender)]))

#ESTER2
sum(na.exclude(speakers$ester2[speakers$gender=="male"]))
sum(na.exclude(speakers$ester2[speakers$gender=="female"]))
sum(na.exclude(speakers$ester2[is.na(speakers$gender)]))

#ETAPE
sum(na.exclude(speakers$etape[speakers$gender=="male"]))
sum(na.exclude(speakers$etape[speakers$gender=="female"]))
sum(na.exclude(speakers$etape[is.na(speakers$gender)]))

#REPERE
sum(na.exclude(speakers$repere[speakers$gender=="male"]))
sum(na.exclude(speakers$repere[speakers$gender=="female"]))
sum(na.exclude(speakers$repere[is.na(speakers$gender)]))


# === HISTOGRAMMES ======================================================================

#Aperçu des données, elles semblent effectivement ne pas suivre une
#loi normale, mais plutôt une loi de Poisson

# Certains locuteurs ne parlent pas, all = 0
# On extrait dans un dataframe, le nombre de tour par locuteur sur l'ensemble des corpus
# en excluant les locuteurs ne parlant pas
spk_all<-subset(speakers,speakers$all>0,select=c("id_speaker","gender","all"))

# Histogramme simple
ggplot(spk_all,aes(x=spk_all$all,fill=spk_all$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 5)+
  labs(title="Turn count per speaker",x="Number of turns",y="Frequency",fill="Gender")

# Peu lisible on n'affiche que jusqu'à 500 en abscisse et 2000 en ordonnée
ggplot(spk_all,aes(x=spk_all$all,fill=spk_all$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 5)+
  coord_cartesian(xlim=c(0,500),ylim=c(0,2000))+
  labs(title="Turn count per speaker (<500)",x="Number of turns",y="Frequency",fill="Gender")

# On zoome sur la fenêtre 100-300 on aura plutôt des invités ou des experts

ggplot(spk_all,aes(x=spk_all$all,fill=spk_all$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 5)+
  coord_cartesian(xlim=c(100,300),ylim=c(0,10))+
  labs(title="Turn count per speaker (100<x<300)",x="Number of turns",y="Frequency",fill="Gender")

#=> beaucoup plus d'hommes que de femmes

# Sur la fenêtre 300-1500, présentateurs ou personnalité publique forte (politiques)

ggplot(spk_all,aes(x=spk_all$all,fill=spk_all$gender))+
  geom_histogram(alpha=0.75,position="identity",binwidth = 5)+
  coord_cartesian(xlim=c(300,1500),ylim=c(0,10))+
  labs(title="Turn count per speaker (300<x<1500)",x="Number of turns",y="Frequency",fill="Gender")

#=> beaucoup plus d'hommes également

# === BOXPLOTS =============================================================================

ggplot(spk_all,aes(x='',y=spk_all$all, fill=spk_all$gender))+geom_boxplot()+
  scale_fill_discrete(name="Gender")+
  labs(title="Distribution of the number of turn per speaker by gender",x="",y="Turn count per speaker")+
  scale_y_log10()

# On ne voit pas grand chose sur le premier box-plot, hormis la valeur extrême chez les hommes
# à plus de 6000 et également la différence sur les queues de distribution : beaucoup plus grande
# chez les hommes, ce qui laisse supposer qu'ils se retrouvent dans différents rôle de locuteur

# = COMPARAISON PAR CORPUS =================================================================

# Comparaison corpus par corpus 

library(reshape2)
gspeakers<-subset(speakers,speakers$gender!="NA")
speakers_long <- melt(gspeakers, id.vars=c("id_speaker","gender"))

ggplot(speakers_long, aes(x=factor(gender),y=value,fill=factor(gender)))+
  geom_boxplot() + labs(title="Turn per speaker by corpus",fill="Gender",x="",y="Turn count") +
  facet_wrap(~variable)
  
ggplot(speakers_long, aes(x=factor(gender),y=value,fill=factor(gender)))+
  geom_boxplot() + labs(title="Turn count per speaker by corpus",fill="Gender",x="",y="Turn count") +
  facet_wrap(~variable)+
  coord_cartesian(ylim=c(0,1000))

# On observe des queues de distributions plus longues et plus denses chez les hommes
# Ce qui laisse supposer qu'il y a plus d'hommes qui parlent souvent que de femmes (valeurs extrêmes)
# Car la majorité des locuteurs ont peu de tours de parole (cf histogramme)

# === REGRESSION DE POISSON =================================================================

# Création du modèle global (on ne prend pas en compte les locuteurs ne parlant pas)
# On utilise un modèle de poisson : la variable à expliquer est le nombre de tour par locuteur
# et la variable explicative est le genre
fit<-glm(spk_all$all~spk_all$gender,family=poisson())
summary(fit)

