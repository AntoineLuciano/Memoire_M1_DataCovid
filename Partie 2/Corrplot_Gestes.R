rm(list = ls())

library(dplyr)
library(corrplot)

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
df_gestes_corrplot <- CSVtoR()
colnames(df_gestes_corrplot) = c("lavageMain","gelHydroAlcoolique","tousserCoude","mouchoireUnique","distance1M","eviteSerrerMainEmbrasser","eviteRegroupements","resterChezSoi","porterMasque","vague")

# Figure 4 : Corrplot des variables gestes barrières et vague
corrplot(cor(df_gestes_corrplot), type="upper", order="hclust", tl.col="black", tl.srt=45)
