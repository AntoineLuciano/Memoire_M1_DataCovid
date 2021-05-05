rm(list = ls())

library(dplyr)
library(WeightedCluster)
library(dendextend)

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
names_modalites <- c("Tout le temps", "Souvent", "Parfois", "Rarement", 
                     "Jamais même si je pourrais le faire", 
                     "Jamais car je n’ai pas la possibilité de le faire")
var <- c("B2_0", "B2_1", "B2_2", "B2_3", "B2_4", "B2_5", "B2_6", "B2_7", "B2_8")
df_CAH <- CSVtoR(var)
colnames(df_CAH) <- c("lavageMain", "gelHydroAlcoolique", "tousserCoude",
                      "mouchoireUnique", "distance1M", "eviteSerrerMainEmbrasser",
                      "eviteRegroupements", "resterChezSoi", "porterMasque", "vague")
df_CAH <- filter(df_CAH, porterMasque != 7 & lavageMain != 7 & gelHydroAlcoolique != 7
& tousserCoude != 7 & mouchoireUnique != 7 & distance1M != 7
& eviteSerrerMainEmbrasser != 7 & eviteRegroupements != 7
& resterChezSoi != 7)


table_CAH <- data.frame(porterMasque = unclass(table(df_CAH$porterMasque)))
table_CAH$lavageMain <- unclass(table(df_CAH$lavageMain))
table_CAH$tousserCoude <- unclass(table(df_CAH$tousserCoude))
table_CAH$gelHydroAlcoolique <- unclass(table(df_CAH$gelHydroAlcoolique))
table_CAH$mouchoireUnique <- unclass(table(df_CAH$mouchoireUnique))
table_CAH$distance1M <- unclass(table(df_CAH$distance1M))
table_CAH$eviteSME <- unclass(table(df_CAH$eviteSerrerMainEmbrasser))
table_CAH$eviteRegroupements <- unclass(table(df_CAH$eviteRegroupements))
table_CAH$resterChezSoi <- unclass(table(df_CAH$resterChezSoi))

rownames(table_CAH) <- names_modalites
table_CAH <- t(table_CAH)
# Figure 13 : Matrice des effectifs, avec les gestes barrières en ligne et les modalités en colonne
View(table_CAH)

# Figure 14 : Matrice des effectifs, centrée réduite
table_CAH.cr <- t(round(scale(t(table_CAH), center = T, scale = T), 3))
View(table_CAH.cr)


# Figure 15 : Matrice des distances entre chaque geste
table_CAH.dist <- dist(table_CAH.cr, method = "euclidean")
dst <- data.matrix(table_CAH.dist)
dst <- apply(dst, 2, rev)
dim <- ncol(dst)
par(mar = c(8, 8, 1, 1))
image(1:dim, 1:dim, dst, axes = FALSE, xlab = "", ylab = "")
axis(1, 1:dim, rev(rownames(table_CAH.cr)), cex.axis = 0.9, las = 3)
axis(2, 1:dim, (rownames(table_CAH.cr)), cex.axis = 0.9, las = 1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex = 1.1)

# Figure 16 : Dendogramme représentant la partition de nos gestes barrières
par(mar = c(4, 4, 2, 2))
cah.ward <- hclust(table_CAH.dist, method = "ward.D2")
plot(cah.ward, xlab = "Gestes barrières", ylab = "Niveau de regroupement",
     main = "Dendogramme des gestes barrières")


# Figure 19 : Valeur de la mesure ASW selon le nombre de groupes retenus
ASW.stats <- numeric(8)
for (i in 2:9) {
  typo.i <- cutree(cah.ward, i)
  clustqual4 <- wcClusterQuality(table_CAH.dist, typo.i)
  ASW.stats[i - 1] <- clustqual4$stats["ASW"]
}
par(mar = c(6, 4, 2, 2))
plot(2:9, ASW.stats, main = "Evolution du critère ASW selon le nombre de groupes",
     xlab = "Nombre de groupes", ylab = "ASW")
abline(h = ASW.stats[3], col = "red")
legend(2, 0.95,
  legend = "ASW = 0.622",
  col = "red", lty = 1, cex = 0.8
)


# Figure 20 : Évolution des 4 coefficients de mesure de la qualité de la partition selon le nombre de groupes retenus
wardRange <- as.clustrange(cah.ward,
  diss = table_CAH.dist, ncluster = 9
)
plot(wardRange, stat = c("ASW", "HG", "PBC","HC"), 
     xlab = "Nombre de groupes dans le regroupement", ylab = "Valeur du critère", 
     legend = FALSE, col = c("chartreuse4", "coral1", "cyan4", "darkkhaki")) 
    legend(7, 0.4, legend = c("HG", "ASW", "PBC", "HC"), 
    col = c("coral1", "chartreuse4", "cyan4", "darkkhaki"), lty = 1, 
    cex = 0.8, text.width = 1
)


# Figure 21 : Regroupement en 3 groupes des 9 gestes barrières
par(mar = c(10, 4, 4, 2))
dend <- color_branches(cah.ward, k = 3, col = c("red", "blue", "green"))
plot(dend, xlab = "", ylab = "Niveau de regroupement", main = "Partition en 3 groupes")
