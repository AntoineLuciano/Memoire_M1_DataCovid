rm(list = ls())

library(dplyr)

# Récupérer les données
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
df_gestes_groupes <- CSVtoR(var)
names_col <- c("lavageMain", "gelHydroAlcoolique", "tousserCoude", 
               "mouchoireUnique", "distance1M", "eviteSerrerMainEmbrasser", 
               "eviteRegroupements", "resterChezSoi", "porterMasque", "vague")
colnames(df_gestes_groupes) <- names_col

# On retire les non réponses
df_gestes_groupes <- filter(df_gestes_groupes, porterMasque != 7 & lavageMain != 7 
                            & gelHydroAlcoolique != 7 & tousserCoude != 7 
                            & mouchoireUnique != 7 & distance1M != 7
                            & eviteSerrerMainEmbrasser != 7 & eviteRegroupements != 7
                            & resterChezSoi != 7)

# Dichotomisation des variables
df_gestes_groupes$lavageMain <- as.integer(df_gestes_groupes$lavageMain <= 2)
df_gestes_groupes$gelHydroAlcoolique <- as.integer(df_gestes_groupes$gelHydroAlcoolique <= 2)
df_gestes_groupes$tousserCoude <- as.integer(df_gestes_groupes$tousserCoude <= 2)
df_gestes_groupes$mouchoireUnique <- as.integer(df_gestes_groupes$mouchoireUnique <= 2)
df_gestes_groupes$distance1M <- as.integer(df_gestes_groupes$distance1M <= 2)
df_gestes_groupes$eviteSerrerMainEmbrasser <- as.integer(df_gestes_groupes$eviteSerrerMainEmbrasser <= 2)
df_gestes_groupes$eviteRegroupements <- as.integer(df_gestes_groupes$eviteRegroupements <= 2)
df_gestes_groupes$resterChezSoi <- as.integer(df_gestes_groupes$resterChezSoi <= 2)
df_gestes_groupes$porterMasque <- as.integer(df_gestes_groupes$porterMasque <= 2)

# Création des groupes de gestes
df_gestes_groupes$groupe1 <- df_gestes_groupes$resterChezSoi 
                                + df_gestes_groupes$gelHydroAlcoolique
df_gestes_groupes$groupe2 <- df_gestes_groupes$porterMasque
df_gestes_groupes$groupe3 <- rowSums(subset(df_gestes_groupes, 
                                    select = -c(porterMasque, gelHydroAlcoolique, 
                                                resterChezSoi, vague, groupe1, groupe2)))

df_gestes_groupes <- df_gestes_groupes[c("groupe1", "groupe2", "groupe3")]

# Figure 22 : Tables du respect des 3 groupes de gestes barrières
table_groupes <- t(data.frame(
  groupe1 = c(unclass(table(df_gestes_groupes$groupe1)), NA, NA, NA, NA),
  groupe2 = c(unclass(table(df_gestes_groupes$groupe2)), NA, NA, NA, NA, NA),
  groupe3 = unclass(table(df_gestes_groupes$groupe3))
))
colnames(table_groupes) <- c("Niveau 1", "Niveau 2", "Niveau 3", "Niveau 4",
                             "Niveau 5", "Niveau 6", "Niveau 7")

View(table_groupes)
