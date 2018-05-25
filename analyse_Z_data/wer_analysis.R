setwd("/home/getalp/garnerim/Bureau/analyse_Z_data")
library(readr)
library(ggplot2)

dev<- read_delim("Dev-prediction_all-info.csv", "\t", escape_double = FALSE, 
                 col_types = cols(gender = col_factor(levels = c("homme","femme")), 
                                  native = col_factor(levels = c("natif","non_natif")),
                                  show = col_factor(levels = c("tvme","africa1", "csoj.16k", "BFMTV-CultureEtVous", "BFMTV-PlaneteShowbiz", "ARTE-NEWS","FINTER-FABHISTOIRE-POD", "FCULT-TEMPS-POD", "TV8-LaPlaceDuVillage")), 
                                  type = col_factor(levels = c("spontanée","non_spontanée"))), 
                 trim_ws = TRUE)

train<-read_delim("Train-prediction_all-info.csv","\t", escape_double = FALSE, 
                  col_types = cols(gender = col_factor(levels = c("homme","femme")), 
                                   native = col_factor(levels = c("natif","non_natif")),
                                   show = col_factor(levels = c("rfi-fm-dga","RFI-ELDA","RTM-ELDA","EST2BC-FRE-FR-FINTER-DEBATE","LCP-PileEtFace","QRBC-FRE-FR-FRANCE3-DEBATE","QRBC-FRE-FR-FINTER-DEBATE","QRBC-FRE-FR-FINTER-TELSONNE-POD","TELSONNE")), 
                                   type = col_factor(levels = c("spontanée","non_spontanée"))), 
                  trim_ws = TRUE)

show_dev<-unique(dev$show)
show_train<-unique(train$show)

# ===== DEV ANALYSIS ================================================================

#Etude des longueurs des tours de paroles 

dev_lengths<-dev$end_time-dev$start_time
dev_gender<-dev$gender

dev_glengths<-data.frame(dev_lengths,dev_gender)
colnames(dev_glengths)<-c("length","gender")

length_mean_m_dev<-mean(dev_glengths$length[dev_glengths$gender=="homme"],rm.na=TRUE)
length_mean_f_dev<-mean(dev_glengths$length[dev_glengths$gender=="femme"],rm.na=TRUE)

# boxplot(dev_glengths$length~dev_glengths$gender,main="Longueur des tours de parole en fonction du genre (DEV)",horizontal=TRUE)

#Etude des WER

wer_mean_m_dev<-mean(dev$WER[dev$gender=="homme"])
wer_mean_f_dev<-mean(dev$WER[dev$gender=="femme"])
wer_sd_m_dev<-sd(dev$WER[dev$gender=="homme"])
wer_sd_f_dev<-sd(dev$WER[dev$gender=="femme"])

# boxplot(dev$WER~dev$gender,main="WER en fonction du genre (DEV)",horizontal=TRUE)

# Comparaison corpus par corpus 

gdev<-subset(dev,dev$gender!="NA",select=c("show","speaker","gender","WER"))

ggplot(gdev, aes(x=gdev$gender,y=gdev$WER,fill=gdev$gender))+
  geom_boxplot() + labs(title="WER by gender by show (DEV)",fill="Gender",x="",y="")+
  coord_cartesian(ylim=c(0,100))

ggplot(gdev, aes(x=gdev$gender,y=gdev$WER,fill=gdev$gender))+
  geom_boxplot() + labs(title="WER by gender by show (DEV)",fill="Gender",x="",y="Turn count") +
  facet_wrap(~gdev$show)+
  coord_cartesian(ylim=c(0,100))


# ===== TRAIN ANALYSIS ================================================================

#Etude des WER

wer_mean_m_train<-mean(train$WER[train$gender=="homme"])
wer_mean_f_train<-mean(train$WER[train$gender=="femme"])
wer_sd_m_train<-sd(train$WER[train$gender=="homme"])
wer_sd_f_train<-sd(train$WER[train$gender=="femme"])

#Etude des longueurs des tours de paroles 

train_lengths<-train$end_time-train$start_time
train_gender<-train$gender

train_glengths<-data.frame(train_lengths,train_gender)
colnames(train_glengths)<-c("length","gender")

length_mean_m_train<-mean(train_glengths$length[train_glengths$gender=="homme"],rm.na=TRUE)
length_mean_f_train<-mean(train_glengths$length[train_glengths$gender=="femme"],rm.na=TRUE)


# =================== ETUDE PAR EMISSIONS ===================================================================

train_rfi<-subset(train,train$show=="rfi-fm-dga")
train_rfi_elda<-subset(train,train$show=="RFI-ELDA")
train_rtm<-subset(train,train$show=="RTM-ELDA")
train_e2_finter<-subset(train,train$show=="EST2BC-FRE-FR-FINTER-DEBATE")
train_pileface<-subset(train,train$show=="LCP-PileEtFace")
train_fr3<-subset(train,train$show=="QRBC-FRE-FR-FRANCE3-DEBATE")
train_qr_finter<-subset(train,train$show=="QRBC-FRE-FR-FINTER-DEBATE")
train_qr_telsonne<-subset(train,train$show=="QRBC-FRE-FR-FINTER-TELSONNE-POD")
train_telsonne<-subset(train,train$show=="TELSONNE")

gtrain<-subset(train,train$gender!="NA",select=c("show","speaker","gender","WER"))

ggplot(gtrain, aes(x=gtrain$gender,y=gtrain$WER,fill=gtrain$gender))+
  geom_boxplot() + labs(title="WER by gender by show (TRAIN)",fill="Gender",x="",y="Turn count") +
  facet_wrap(~gtrain$show)+
  coord_cartesian(ylim=c(0,100))


dev_plshowbiz<-subset(dev,dev$show=="BFMTV-PlaneteShowbiz")
