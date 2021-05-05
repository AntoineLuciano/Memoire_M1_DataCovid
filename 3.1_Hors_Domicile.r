library(ordinal)
library(ggplot2)
library(mgsub)
library(questionr)
library(gtsummary)

CSVtoR_avec_Rev <- function(var){
  setwd(dir = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DATA"))
  V <- read.csv("Datacovid_BD_Vague_1.csv",sep=";",dec=".")
  df <- V[var]
  df$VAGUE <- 1
  for (i in 2:7){
    file= paste("Datacovid_BD_Vague_",".csv",sep=as.character(i))
    V <- read.csv(file,sep=";",dec=".")
    extr <- V[var]
    extr$VAGUE <- i
    df <- rbind(df,extr)
  }
  V <- read.csv("Datacovid_BD_Vague_8.csv",sep=";",dec=".")
  extr <- V[c(var, "Q45")]
  extr$VAGUE <- 8
  df$Q45 <- NA
  df <- rbind(df,extr)
  return(df)
}

# Préparation des données
var= c("AGE2","PCSI","SEXE","EMP","C10","C6_0","C6_1","C6_2","C6_3", "B2_0","B2_1","B2_2","B2_3","B2_4","B2_5","B2_6","B2_7","B2_8")
df <- CSVtoR_avec_Rev(var)
names_col = c("Age","PCSI","Sexe","Emp","C10","C6_0","C6_1","C6_2","C6_3","lavageMain","gelHydroAlcoolique","tousserCoude","mouchoireUnique","distance1M","eviteSerrerMainEmbrasser","eviteRegroupements","resterChezSoi","porterMasque","Vague","Revenus")
colnames(df) = names_col

# Dichotomisation des variables
df$lavageMain1 <- as.integer(df$lavageMain<=2)
df$gelHydroAlcoolique1 <- as.integer(df$gelHydroAlcoolique<=2)
df$tousserCoude1 <- as.integer(df$tousserCoude<=2)
df$mouchoireUnique1 <- as.integer(df$mouchoireUnique<=2)
df$distance1M1 <- as.integer(df$distance1M<=2)
df$eviteSerrerMainEmbrasser1 <- as.integer(df$eviteSerrerMainEmbrasser<=2)
df$eviteRegroupements1 <- as.integer(df$eviteRegroupements<=2)
df$resterChezSoi1 <- as.integer(df$resterChezSoi<=2)
df$porterMasque1 <- as.integer(df$porterMasque<=2)

# Création des groupes de gestes
df$groupe1<-df$resterChezSoi1+df$gelHydroAlcoolique1
df$groupe2<-df$porterMasque1
df$groupe3<-rowSums(df[,c("lavageMain1","tousserCoude1","mouchoireUnique1","distance1M1","eviteSerrerMainEmbrasser1","eviteRegroupements1")])

df$groupe1 <- as.factor(df$groupe1)
df$groupe2 <- as.factor(df$groupe2)
df$groupe3 <- as.factor(df$groupe3)

df$groupe1[df$porterMasque==7 | df$lavageMain==7 | df$gelHydroAlcoolique==7 | df$tousserCoude == 7 | df$mouchoireUnique == 7 | df$distance1M == 7 | df$eviteSerrerMainEmbrasser == 7 | df$eviteRegroupements == 7 | df$resterChezSoi == 7] <- NA
df$groupe2[df$porterMasque==7 | df$lavageMain==7 | df$gelHydroAlcoolique==7 | df$tousserCoude == 7 | df$mouchoireUnique == 7 | df$distance1M == 7 | df$eviteSerrerMainEmbrasser == 7 | df$eviteRegroupements == 7 | df$resterChezSoi == 7] <- NA
df$groupe3[df$porterMasque==7 | df$lavageMain==7 | df$gelHydroAlcoolique==7 | df$tousserCoude == 7 | df$mouchoireUnique == 7 | df$distance1M == 7 | df$eviteSerrerMainEmbrasser == 7 | df$eviteRegroupements == 7 | df$resterChezSoi == 7] <- NA


#Sexe
df$Sexe[which(df$Sexe==1)] <- "Homme"
df$Sexe[which(df$Sexe==2)] <- "Femme"

#Age
df$Age <- mgsub(df$Age,pattern=1:10,replacement=c("18-29","18-29","30-39","30-39","40-49","40-49","50-59","50-59","60+","60+"))
df$Age <- as.factor(df$Age)

#PCSI
metier_cat=c("Agriculteurs","Indépendants","Cadres sups","Prof. intermédiaidf","Employés","Ouvriers","Retraités","Inactifs")
df$PCSI <- as.character(df$PCSI)
df$PCSI<-mgsub(df$PCSI,pattern=c("1","2","3","4","5","6","7","8"),replacement=metier_cat)
df$PCSI <- relevel(as.factor(df$PCSI), ref="Cadres sups")

#HORS DOMICILE
df$HorsDomicile <- NA 
df$HorsDomicile[which(!is.na(df$C6_0))] <- 0
df$HorsDomicile[which(df$C6_0 == 1 |df$C6_1 == 1 | df$C6_2 == 1 | df$C6_3 == 1 )] <- 1
df$HorsDomicile <- as.factor(df$HorsDomicile)

#VARIABLE PROF DE Sante
df$ProfSante=NA
df$ProfSante[which(!is.na(df$C6_0))] <- "Non"
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
df$Diplome <- factor(df$Diplome, levels=c("Sans Diplome", "- de BAC", "BAC à BAC+2", "BAC+3 ou +","Autre"))

#VARIABLE REVENUS
rev_seuil=c("1000€ ou -","1000€ ou -","1000€ ou -","1000-2000€","1000-2000€","2000-3000€","3000€ ou +",NA)
df$Revenus<-mgsub(df$Revenus,pattern=1:8,replacement=rev_seuil)
df$Revenus <- as.factor(df$Revenus)
df$Revenus <- relevel(as.factor(df$Revenus), ref="1000€ ou -")
df$Revenus <- factor(df$Revenus, levels=c("1000€ ou -","1000-2000€","2000-3000€","3000€ ou +"))


#HD expliqué par les GB
reg_GB=glm(formula = HorsDomicile ~ groupe1 + groupe2 + groupe3, family= binomial(link='logit'), data=df)

tbl_regression(reg_GB, exponentiate = TRUE)

#HD expliqué par PCSI AGE DIPLOME ETC
reg= glm(formula = HorsDomicile ~  PCSI + Sexe + Age + Diplome + ProfSante, family=binomial(link=logit),data= df)

tbl_regdfsion(reg, exponentiate = TRUE)

#HD expliqué par les Rev
reg_Rev= glm(formula = HorsDomicile ~  Revenus, family=binomial(link=logit),data= df)
tbl_regdfsion(reg_Rev, exponentiate = TRUE)


#FIGURE 23
table_HD<-t(data.frame(Nombre = as.character(unclass(table(df$HorsDomicile))), 
                       Frequence = paste(as.character(round(unclass(table(df$HorsDomicile))/sum(!is.na(df$HorsDomicile))*100,3)),"%")))
colnames(table_HD)<-c("Travaille chez soi", "Travaille à l'extérieur")

View(table_HD)

#FIGURE 24
HD <- numeric(8)

for(i in 1:8){
  HD[i] <- sum(as.integer(df$HorsDomicile[df$Vague ==i & !is.na(df$HorsDomicile)]))/length(!is.na(df$HorsDomicile))*100
}

date = as.Date(c("2020-04-07","2020-04-15","2020-04-22","2020-04-28","2020-05-05","2020-05-12","2020-05-26","2020-06-05"))

graph <- data.frame(date)
graph <- cbind(graph,HD)

g <- qplot(date,HD, data=graph, geom=c("point","path"), main="Evolution de la proportion de travailleurs Hors Domicile", ylab = "Proportion de travailleurs Hors Domicile (en %)",xlab="Date") 
g <- g + geom_vline(xintercept=as.Date("2020-05-11"), col="red")
g + geom_text( x=as.Date("2020-04-27") , y=10.9, label="Déconfinement du 11 mai 2020", color="red",size = 3.7, fontface=1)

