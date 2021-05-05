rm(list = ls())

library(dplyr)
library(corrplot)

# Récupération des données
CSVtoR <- function(var) {
  setwd(dir = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DATA"))
  V <- read.csv("Datacovid_BD_Vague_1.csv", sep = ";", dec = ".")
  df <- V[var]
  df$VAGUE <- 1
  for (i in 2:8) {
    file <- paste("Datacovid_BD_Vague_", ".csv", sep = as.character(i))
    V <- read.csv(file, sep = ";", dec = ".")
    extr <- V[var]
    extr$VAGUE <- i
    df <- rbind(df, extr)
  }
  return(df)
}

# Préparation des données
var <- c("B2_0", "B2_1", "B2_2", "B2_3", "B2_4", "B2_5", "B2_6", "B2_7", "B2_8")
df_gestes_corrplot <- CSVtoR(var)
colnames(df_gestes_corrplot) <- c("lavageMain", "gelHydroAlcoolique", "tousserCoude", "mouchoireUnique", "distance1M", "eviteSerrerMainEmbrasser", "eviteRegroupements", "resterChezSoi", "porterMasque", "vague")

df_gestes_corrplot <- filter(df_gestes_corrplot, porterMasque != 7 & lavageMain != 7 
                      & gelHydroAlcoolique != 7 & tousserCoude != 7 
                      & mouchoireUnique != 7 & distance1M != 7
                      & eviteSerrerMainEmbrasser != 7 & eviteRegroupements != 7
                      & resterChezSoi != 7)


# Figure 12 : Corrplot des variables gestes barrières et vague
corrplot(cor(df_gestes_corrplot), type = "upper", order = "hclust",
                  tl.col = "black", tl.srt = 45, diag = FALSE)
