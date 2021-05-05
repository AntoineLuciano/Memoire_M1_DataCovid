library(ordinal)
library(VGAM)
library(ggplot2)
library(mgsub)
library(questionr)
library(gtsummary)

CSVtoR <- function(var){
  setwd(dir = "/Users/antoi/OneDrive/Documents/Cours/Mémoire/Data")
  V <- read.csv("Datacovid_BD_Vague_1.csv",sep=";",dec=".")
  df <- V[var]
  df$VAGUE = 1
  for (i in 2:8){
    file= paste("Datacovid_BD_Vague_",".csv",sep=as.character(i))
    V <- read.csv(file,sep=";",dec=".")
    extr <- V[var]
    extr$VAGUE = i
    df <- rbind(df,extr)
  }
  return(df)
}

# Préparation des données
var= c("AGE2","PCSI","SEXE","EMP","C10","A1","A2","C5_0","C5_1","C5_2","C5_3","C6_0","C6_1","C6_2","C6_3")
df <- CSVtoR(var)
names_col = c("Age","PCSI","Sexe","Emp","C10","A1","A2","C5_0","C5_1","C5_2","C5_3","C6_0","C6_1","C6_2","C6_3","Vague")
colnames(df) = names_col

#Sexe
df$Sexe[which(df$Sexe==1)] <- "Homme"
df$Sexe[which(df$Sexe==2)] <- "Femme"

#Age
df$Age <- mgsub(df$Age,pattern=1:10,replacement=c("18-29","18-29","30-39","30-39","40-49","40-49","50-59","50-59","60+","60+"))
df$Age <- as.factor(df$Age)

#PCSI + EMP
metier_cat=c("Agriculteurs","Indépendants","Caddf sups","Prof. intermédiaidf","Employés","Ouvriers","Retraités","Inactifs")
df$PCSI<-mgsub(df$PCSI,pattern=1:8,replacement=metier_cat)
df$PCSI[df$PCSI == "Inactifs" & df$Emp==5] <- "Sans Emploi"
df$PCSI[df$PCSI == "Inactifs" & df$Emp==6] <- "H/F au Foyer"
df$PCSI[df$PCSI == "Inactifs" & df$Emp==7] <- "Retraités"
df$PCSI[df$PCSI == "Inactifs" & df$Emp==8] <- "Etudiants"
df$PCSI <- relevel(as.factor(df$PCSI), ref="Caddf sups")

#HORS DOMICILE
df$HorsDomicile <- NA 
df$HorsDomicile[which(!is.na(df$C6_0))] <- 0
df$HorsDomicile[df$PCSI=="Sans Emploi" | df$PCSI =="H/F au Foyer" | df$PCSI =="Etudiants" | df$PCSI == "Retraités"] <- 0
df$HorsDomicile[which(df$C6_0 == 1 |df$C6_1 == 1 | df$C6_2 == 1 | df$C6_3 == 1 )] <- 1
df$HorsDomicile <- as.factor(df$HorsDomicile)

#VARIABLE PROF DE Sante
df$ProfSante <- NA
df$ProfSante[which(!is.na(df$C6_0))] <- "Non"
df$ProfSante[df$PCSI=="Sans Emploi" | df$PCSI =="H/F au Foyer" | df$PCSI =="Etudiants" | df$PCSI == "Retraités"] <- "Non"
df$ProfSante[which(df$C6_0 == 8 |df$C6_1 == 8 | df$C6_2 == 8 | df$C6_3 == 8 )] <- "Oui"
df$ProfSante <- as.factor(df$ProfSante)

#VARIABLE DIPLOME
df$Diplome <- NA
df$Diplome[which(df$C10==1)] <- "Sans Diplome"
df$Diplome[which(df$C10==2 | df$C10==3)] <- "- de BAC"
df$Diplome[which(df$C10 >=4 & df$C10 <= 5)] <- "BAC à BAC+2"
df$Diplome[which(df$C10 == 6)] <- "BAC+3 ou +"
df$Diplome[which(df$C10 == 7)] <- NA
df$Diplome <- as.factor(df$Diplome)
df$Diplome <- relevel(as.factor(df$Diplome), ref="Sans Diplome")
df$Diplome <- factor(df$Diplome, levels=c("Sans Diplome", "- de BAC", "BAC à BAC+2", "BAC+3 ou +"))

#VARIABLE INFECTE
df$Infecte <- NA
df$Infecte[which(df$A1 == 2)] <- "N'a pas contracté le virus"
df$Infecte[which(df$A2 <3)] <- "A contracté le virus"
df$Infecte[which(df$A2 >2 && df$A2 == 7)] <- "N'a pas contracté le virus"
df$Infecte <- as.factor(df$Infecte)
df$Infecte <- relevel(df$Infecte, ref="N'a pas contracté le virus")

#SCORE PHQ
df$PHQ <- 0
df$PHQ <- df$PHQ+4-df$C5_0
df$PHQ <- df$PHQ+4-df$C5_1
df$PHQ <- df$PHQ+4-df$C5_2
df$PHQ <- df$PHQ+4-df$C5_3

df$PHQ[which(df$C5_0 ==5 | df$C5_1 ==5 | df$C5_2 ==5 | df$C5_3 ==5)] <- NA

#PROBIT ORDONNE
df$PHQ_seuil <- mgsub(df$PHQ,pattern=0:12,replacement=c(1,1,1,2,2,2,3,3,3,4,4,4,4))

sans_pente <- vglm(formula = PHQ_seuil ~ HorsDomicile + Sexe + Age + PCSI + Diplome + ProfSante, data = df, cumulative(parallel=FALSE, reverse=TRUE ))
#summary(sans_pente)

avec_pente <- vglm(formula = PHQ_seuil ~ HorsDomicile + Sexe + Age + PCSI + Diplome + ProfSante, data = df, cumulative(parallel=TRUE,reverse=TRUE))
#summary(avec_pente)

#TEST D'EGALITE DES PENTES
S <- -2*(logLik(avec_pente)-logLik(sans_pente))
pval <- 1-pchisq(S,df=length(coef(sans_pente))-length(coef(avec_pente)))
pval

#BARPLOT COEFF HORS DOMICILE
coef_HD = numeric(3)
coef_HD[1] = as.numeric(coef(sans_pente)["HorsDomicile1:1"])
coef_HD[2] = as.numeric(coef(sans_pente)["HorsDomicile1:2"])
coef_HD[3] = as.numeric(coef(sans_pente)["HorsDomicile1:3"])
barplot(coef_HD, main= "Décroissance du coéfficient de HorsDomicile",ylab="Coeff de HorsDomicile",names.arg=c("Non -> Benigne","Benigne -> Modérée","Modérée -> Sévère"))

#REGdfSION LOGISTIQUE
df$PHQ4 <-mgsub(df$PHQ,pattern=0:12,replacement=c(0,0,0,0,0,0,1,1,1,1,1,1,1))
df$PHQ4 <- as.factor(df$PHQ4)

reg <- glm(formula = PHQ4 ~ PCSI + HorsDomicile + Sexe + Age + Diplome + Infecte + ProfSante, data = df, family=binomial)

tbl_regdfsion(reg, exponentiate = TRUE)

#GRAPHIQUE EVOL PHQ
PHQ <- numeric(8)
None <- numeric(8)
Benigne <- numeric(8)
Mod <- numeric(8)
Sev <-  numeric(8)
moy <- numeric(8)
for(i in 1:8){
  None[i] <- length(df$PHQ_seuil[df$Vague ==i & df$PHQ_seuil == 1])/length(df$PHQ_seuil[df$Vague ==i & !is.na(df$PHQ_seuil)])
  Benigne[i] <- length(df$PHQ_seuil[df$Vague ==i & df$PHQ_seuil == 2])/length(df$PHQ_seuil[df$Vague ==i & !is.na(df$PHQ_seuil)])
  Mod[i] <- length(df$PHQ_seuil[df$Vague ==i & df$PHQ_seuil == 3])/length(df$PHQ_seuil[df$Vague ==i & !is.na(df$PHQ_seuil)])
  Sev[i] <- length(df$PHQ_seuil[df$Vague ==i & df$PHQ_seuil == 4])/length(df$PHQ_seuil[df$Vague ==i & !is.na(df$PHQ_seuil)])
  PHQ[i] <- sum(as.integer(df$PHQ[df$Vague==i & !is.na(df$PHQ)] >=3))/length(df$PHQ[df$Vague ==i & !is.na(df$PHQ)])
  moy[i] <- mean(as.numeric(df$PHQ[df$Vague==i & !is.na(df$PHQ)]))
}
graph <- data.frame(Vague=c(1:8))
date = as.Date(c("2020-04-07","2020-04-15","2020-04-22","2020-04-28","2020-05-05","2020-05-12","2020-05-26","2020-06-05"))
graph <- cbind(graph,PHQ,None,Benigne,Mod,Sev,date,moy)

qplot(date,moy, data=graph, geom=c("point","path")) 

