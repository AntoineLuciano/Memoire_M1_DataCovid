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
df_gestes <- CSVtoR(var)
names_col <- c("lavageMain", "gelHydroAlcoolique", "tousserCoude", "mouchoireUnique", "distance1M", "eviteSerrerMainEmbrasser", "eviteRegroupements", "resterChezSoi", "porterMasque", "vague")
colnames(df_gestes) <- names_col


names_modalites <- c("Tout le temps", "Souvent", "Parfois", "Rarement", "Jamais même si je pourrais le faire", "Jamais car je n’ai pas la possibilité de le faire", "Non réponse")
save_vague <- df_gestes$vague
for (i in 1:ncol(df_gestes)) {
  df_gestes[df_gestes == i] <- names_modalites[i]
}
df_gestes$vague <- save_vague

# Création d'une matrice contenant les fréquences de réponses pour chaque GB
table_full_data <- data.frame(unclass(table(df_gestes$lavageMain) / nrow(df_gestes) * 100))
table_full_data$gelHydroAlcoolique <- unclass(table(df_gestes$gelHydroAlcoolique) / nrow(df_gestes) * 100)
table_full_data$tousserCoude <- unclass(table(df_gestes$tousserCoude) / nrow(df_gestes) * 100)
table_full_data$mouchoireUnique <- unclass(table(df_gestes$mouchoireUnique) / nrow(df_gestes) * 100)
table_full_data$distance1M <- unclass(table(df_gestes$distance1M) / nrow(df_gestes) * 100)
table_full_data$eviteSerrerMainEmbrasser <- unclass(table(df_gestes$eviteSerrerMainEmbrasser) / nrow(df_gestes) * 100)
table_full_data$eviteRegroupements <- unclass(table(df_gestes$eviteRegroupements) / nrow(df_gestes) * 100)
table_full_data$resterChezSoi <- unclass(table(df_gestes$resterChezSoi) / nrow(df_gestes) * 100)
table_full_data$porterMasque <- unclass(table(df_gestes$porterMasque) / nrow(df_gestes) * 100)

# On renomme les colonnes de la table
colnames(table_full_data) <- colnames(df_gestes[-10])
# Pour réorganiser l'ordre des lignes
table_full_data <- table_full_data[names_modalites, ]

# Figure 8 : Fréquence de respect des gestes barrières au cours des 8 vagues confondues
View(t(table_full_data))

# Figure 9 : Évolution du port du masque au fil du temps
plot(table(df_gestes$vague, df_gestes$porterMasque)[, "Tout le temps"] / 5000 * 100,
  xlab = "Numéro de la vague", ylab = "Fréquence",
  main = "Fréquence de répondants 'Tout le temps' ou 'Pas la possibilité' selon la vague", col = "blue",
  ylim = c(0, 40), type = "b"
)
points(table(df_gestes$vague, df_gestes$porterMasque)[, "Jamais car je n’ai pas la possibilité de le faire"] / 5000 * 100, col = "red", type = "b")
legend(3.2, 40,
  legend = c("Tout le temps", "N'a pas la possibilité"),
  col = c("blue", "red"), lty = 1, cex = 0.8
)

# Figure 10 : Évolution de l'utilisation de gel hydroalcoolique au fil du temps
plot(table(df_gestes$vague, df_gestes$gelHydroAlcoolique)[, "Tout le temps"] / 5000 * 100,
  xlab = "Numéro de la vague", ylab = "Fréquence",
  main = "Fréquence de répondants 'Tout le temps' ou 'Pas la possibilité' selon la vague", col = "blue",
  ylim = c(0, 40), type = "b"
)
points(table(df_gestes$vague, df_gestes$gelHydroAlcoolique)[, "Jamais car je n’ai pas la possibilité de le faire"] / 5000 * 100, col = "red", type = "b")
legend(3.2, 40,
  legend = c("Tout le temps", "N'a pas la possibilité"),
  col = c("blue", "red"), lty = 1, cex = 0.8
)

# Figure 11 : Évolution du geste tousserCoude au fil du temps
plot(table(df_gestes$vague, df_gestes$tousserCoude)[, "Tout le temps"] / 5000 * 100,
  xlab = "Numéro de la vague", ylab = "Fréquence",
  main = "Fréquence de répondants 'Tout le temps' ou 'Pas la possibilité' selon la vague", col = "blue",
  ylim = c(0, 100), type = "b"
)
points(table(df_gestes$vague, df_gestes$tousserCoude)[, "Jamais car je n’ai pas la possibilité de le faire"] / 5000 * 100, col = "red", type = "b")
legend(3.2, 100,
  legend = c("Tout le temps", "N'a pas la possibilité"),
  col = c("blue", "red"), lty = 1, cex = 0.8
)
