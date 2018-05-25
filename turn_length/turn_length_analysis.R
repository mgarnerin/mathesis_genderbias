#Analyse de la longueur des tours de paroles à l'aide de modèles de Cox

setwd("/home/getalp/garnerim/Bureau/scripts/turn_length")
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

#install.packages("survival")
library("survival")
library(readr)

#Import des données
turns <- read_delim("~/Bureau/clean_data/turn_length/turns_all_info_id_class.csv",";", 
                 escape_double = FALSE, 
                 col_types = cols(gender = col_factor(levels = c("female", "male")), 
                          id_corpus = col_factor(levels = c("1","2", "3", "4")),
                          id_speaker_class = col_factor(levels = c("1","2", "3", "4")),
                          id_episode = col_factor(levels = as.factor(seq(1,675,1))), 
                          id_show = col_factor(levels = as.factor(seq(1,20,1))), 
                          id_speaker = col_factor(levels =as.factor(seq(0,4975,1))), 
                          id_turn = col_character()),
                 trim_ws = TRUE)


# ==== TABLEAUX DES LONGUEURS MOYENNES ET ECARTS-TYPES ===========================================

#Calcul du nombre de tours par genre

turns_by_gender=count(turns,turns$gender=="male")
nb_turn_m=turns_by_gender$n[1]
nb_turn_f=turns_by_gender$n[2]
nb_turn_u=turns_by_gender$n[3]
nb_turn = nb_turn_m+nb_turn_f+nb_turn_u

#Calcul de la longueur moyenne d'un tour de parole par genre

mean_length_m <-mean((turns$end_time-turns$start_time)[turns$gender=="male"],na.rm=TRUE)
mean_length_f <-mean((turns$end_time-turns$start_time)[turns$gender=="female"],na.rm=TRUE)
mean_length_u <-mean((turns$end_time-turns$start_time)[is.na(turns$gender)],na.rm=TRUE)
mean_length <-mean((turns$end_time-turns$start_time),na.rm=TRUE)

sd_length_m <-sd((turns$end_time-turns$start_time)[turns$gender=="male"],na.rm=TRUE)
sd_length_f <-sd((turns$end_time-turns$start_time)[turns$gender=="female"],na.rm=TRUE)
sd_length_u <-sd((turns$end_time-turns$start_time)[is.na(turns$gender)],na.rm=TRUE)
sd_length <-sd((turns$end_time-turns$start_time),na.rm=TRUE)

boxplot((turns$end_time-turns$start_time)~turns$gender)
boxplot(log(turns$end_time-turns$start_time)~turns$gender)

#Idem par corpus :

#ESTER1 :
nb_turns_e1=count(turns,turns$id_corpus==1)$n[2]

nb_turn_e1_m=count(turns,turns$id_corpus==1&turns$gender=="male")$n[2]
nb_turn_e1_f=count(turns,turns$id_corpus==1&turns$gender=="female")$n[2]
nb_turn_e1_u=count(turns,turns$id_corpus==1&is.na(turns$gender))$n[2]

mean_length_e1_m <-mean((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==1],na.rm=TRUE)
mean_length_e1_f <-mean((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==1],na.rm=TRUE)
mean_length_e1_u <-mean((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==1],na.rm=TRUE)
mean_length_e1 <-mean((turns$end_time-turns$start_time)[turns$id_corpus==1],na.rm=TRUE)

sd_length_e1_m <-sd((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==1],na.rm=TRUE)
sd_length_e1_f <-sd((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==1],na.rm=TRUE)
sd_length_e1_u <-sd((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==1],na.rm=TRUE)
sd_length_e1 <-sd((turns$end_time-turns$start_time)[turns$id_corpus==1],na.rm=TRUE)

#ESTER2 :
nb_turns_e2=count(turns,turns$id_corpus==1)$n[2]

nb_turn_e2_m=count(turns,turns$id_corpus==1&turns$gender=="male")$n[2]
nb_turn_e2_f=count(turns,turns$id_corpus==1&turns$gender=="female")$n[2]
nb_turn_e2_u=count(turns,turns$id_corpus==1&is.na(turns$gender))$n[2]

mean_length_e2_m <-mean((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==2],na.rm=TRUE)
mean_length_e2_f <-mean((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==2],na.rm=TRUE)
mean_length_e2_u <-mean((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==2],na.rm=TRUE)
mean_length_e2 <-mean((turns$end_time-turns$start_time)[turns$id_corpus==2],na.rm=TRUE)

sd_length_e2_m <-sd((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==2],na.rm=TRUE)
sd_length_e2_f <-sd((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==2],na.rm=TRUE)
sd_length_e2_u <-sd((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==2],na.rm=TRUE)
sd_length_e2 <-sd((turns$end_time-turns$start_time)[turns$id_corpus==2],na.rm=TRUE)

#ETAPE :
nb_turns_etp=count(turns,turns$id_corpus==1)$n[2]

nb_turn_etp_m=count(turns,turns$id_corpus==1&turns$gender=="male")$n[2]
nb_turn_etp_f=count(turns,turns$id_corpus==1&turns$gender=="female")$n[2]
nb_turn_etp_u=count(turns,turns$id_corpus==1&is.na(turns$gender))$n[2]

mean_length_etp_m <-mean((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==3],na.rm=TRUE)
mean_length_etp_f <-mean((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==3],na.rm=TRUE)
mean_length_etp_u <-mean((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==3],na.rm=TRUE)
mean_length_etp <-mean((turns$end_time-turns$start_time)[turns$id_corpus==3],na.rm=TRUE)

sd_length_etp_m <-sd((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==3],na.rm=TRUE)
sd_length_etp_f <-sd((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==3],na.rm=TRUE)
sd_length_etp_u <-sd((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==3],na.rm=TRUE)
sd_length_etp <-sd((turns$end_time-turns$start_time)[turns$id_corpus==3],na.rm=TRUE)

#REPERE :
nb_turns_rep=count(turns,turns$id_corpus==1)$n[2]

nb_turn_rep_m=count(turns,turns$id_corpus==1&turns$gender=="male")$n[2]
nb_turn_rep_f=count(turns,turns$id_corpus==1&turns$gender=="female")$n[2]
nb_turn_rep_u=count(turns,turns$id_corpus==1&is.na(turns$gender))$n[2]

mean_length_rep_m <-mean((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==4],na.rm=TRUE)
mean_length_rep_f <-mean((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==4],na.rm=TRUE)
mean_length_rep_u <-mean((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==4],na.rm=TRUE)
mean_length_rep <-mean((turns$end_time-turns$start_time)[turns$id_corpus==4],na.rm=TRUE)

sd_length_rep_m <-sd((turns$end_time-turns$start_time)[turns$gender=="male"&turns$id_corpus==4],na.rm=TRUE)
sd_length_rep_f <-sd((turns$end_time-turns$start_time)[turns$gender=="female"&turns$id_corpus==4],na.rm=TRUE)
sd_length_rep_u <-sd((turns$end_time-turns$start_time)[is.na(turns$gender)&turns$id_corpus==4],na.rm=TRUE)
sd_length_rep <-sd((turns$end_time-turns$start_time)[turns$id_corpus==4],na.rm=TRUE)


f_means<-c(mean_length_e1_f,mean_length_e2_f,mean_length_etp_f,mean_length_rep_f,mean_length_f)
m_means<-c(mean_length_e1_m,mean_length_e2_m,mean_length_etp_m,mean_length_rep_m,mean_length_m)
u_means<-c(mean_length_e1_u,mean_length_e2_u,mean_length_etp_u,mean_length_rep_u,mean_length_u)
all_means<-c(mean_length_e1,mean_length_e2,mean_length_etp,mean_length_rep,mean_length)

f_sds<-c(sd_length_e1_f,sd_length_e2_f,sd_length_etp_f,sd_length_rep_f,sd_length_f)
m_sds<-c(sd_length_e1_m,sd_length_e2_m,sd_length_etp_m,sd_length_rep_m,sd_length_m)
u_sds<-c(sd_length_e1_u,sd_length_e2_u,sd_length_etp_u,sd_length_rep_u,sd_length_u)
all_sds<-c(sd_length_e1,sd_length_e2,sd_length_etp,sd_length_rep,sd_length)

means<-data.frame(female=f_means,male=m_means,unknown=u_means,all=all_means,row.names = c("ESTER1","ESTER2","ETAPE","REPERE","ALL"))
sds<-data.frame(female=f_sds,male=m_sds,unknown=u_sds,all=all_sds,row.names = c("ESTER1","ESTER2","ETAPE","REPERE","ALL"))


# ==== MODELES DE SURVIE : ANALYSE DE LA LONGUEUR DES TOURS ===================================

# Création des modèles de survie pour quantifier la différence entre les longueurs de tours

# Ici les effets fixes considérés sont le genre et le corpus, avec un effet aléatoire introduit sur le locuteur
mod<-coxph(Surv(turns$end_time-turns$start_time)~turns$gender*turns$id_corpus+frailty.gaussian(turns$id_speaker,df=1))
summary(mod)

# Dans notre deuxième modèle on ajoute un effet fixe sur la classe du locuteur
mod2<-coxph(Surv(turns$end_time-turns$start_time)~turns$gender*turns$id_corpus*turns$id_speaker_class+frailty.gaussian(turns$id_speaker,df=1))
summary(mod2)


