rm(list = ls())

library(dplyr)

# Récupérer les données
CSVtoR <- function(){
  
  var= c("B2_0","B2_1","B2_2","B2_3","B2_4","B2_5","B2_6","B2_7","B2_8")
  setwd(dir = "/Users/yanisbakhtaoui/Desktop/Mémoire/DATA")
  test <- read.csv("Datacovid_BD_Vague_1.csv",sep=";",dec=".")
  extr <- test[var]
  extr$VAGUE = 1
  
  for (i in 2:8){
    file= paste("Datacovid_BD_Vague_",".csv",sep=as.character(i))
    test <- read.csv(file,sep=";",dec=".")
    extr1 <- test[var]
    extr1$VAGUE = i
    extr <- rbind(extr,extr1)
  }
  return(extr)
}

# Préparation des données 
df_gestes_groupes <- CSVtoR()
names_col = c("lavageMain","gelHydroAlcoolique","tousserCoude","mouchoireUnique","distance1M","eviteSerrerMainEmbrasser","eviteRegroupements","resterChezSoi","porterMasque","vague")
colnames(df_gestes_groupes) = names_col

df_gestes_groupes$lavageMain <- as.integer(df_gestes_groupes$lavageMain>3)
df_gestes_groupes$gelHydroAlcoolique <- as.integer(df_gestes_groupes$gelHydroAlcoolique>3)
df_gestes_groupes$tousserCoude <- as.integer(df_gestes_groupes$tousserCoude>3)
df_gestes_groupes$mouchoireUnique <- as.integer(df_gestes_groupes$mouchoireUnique>3)
df_gestes_groupes$distance1M <- as.integer(df_gestes_groupes$distance1M>3)
df_gestes_groupes$eviteSerrerMainEmbrasser <- as.integer(df_gestes_groupes$eviteSerrerMainEmbrasser>3)
df_gestes_groupes$eviteRegroupements <- as.integer(df_gestes_groupes$eviteRegroupements>3)
df_gestes_groupes$resterChezSoi <- as.integer(df_gestes_groupes$resterChezSoi>3)
df_gestes_groupes$porterMasque <- as.integer(df_gestes_groupes$porterMasque>3)

df_gestes_groupes$groupe1 <- numeric(nrow(df_gestes_groupes))
df_gestes_groupes$groupe2 <- numeric(nrow(df_gestes_groupes))
df_gestes_groupes$groupe3 <- numeric(nrow(df_gestes_groupes))

df_gestes_groupes$groupe1[which((df_gestes_groupes$porterMasque+df_gestes_groupes$gelHydroAlcoolique)/2<0.5)] <- 1
df_gestes_groupes$groupe2<-1-df_gestes_groupes$resterChezSoi
df_gestes_groupes$groupe3[which((rowMeans(subset(df_gestes_groupes,select=-c(porterMasque,gelHydroAlcoolique,resterChezSoi,vague,groupe1,groupe2))))<0.5)] <- 1

df_gestes_groupes <- df_gestes_groupes[c("groupe1","groupe2","groupe3")]

# Figure 13 : Table du respect des 3 groupes de gestes barrières
table_groupes<-rep(1,times=2)
table_groupes<-data.frame(table_groupes)
table_groupes$groupe1<-unclass(table(df_gestes_groupes$groupe1))
table_groupes$groupe2<-unclass(table(df_gestes_groupes$groupe2))
table_groupes$groupe3<-unclass(table(df_gestes_groupes$groupe3))
table_groupes<-t(table_groupes[-1])
colnames(table_groupes) = c("Non","Oui")
View(table_groupes)







