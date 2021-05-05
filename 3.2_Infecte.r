rm(list = ls())

library(mgsub)
library(gtsummary)

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
var <- c("AGE2", "PCSI", "SEXE", "EMP", "C10", "A1", "A2", "C6_0", "C6_1", "C6_2", "C6_3")
df <- CSVtoR(var)
names_col <- c("Age", "PCSI", "Sexe", "Emp", "C10", "A1", "A2", "C6_0", "C6_1",
                  "C6_2", "C6_3", "Vague")
colnames(df) <- names_col

# Sexe
df$Sexe[which(df$Sexe == 1)] <- "Homme"
df$Sexe[which(df$Sexe == 2)] <- "Femme"

# Age
df$Age <- mgsub(df$Age, pattern = 1:10, replacement = c("18-29", "18-29", "30-39",
                        "30-39", "40-49", "40-49", "50-59", "50-59", "60+", "60+"))
df$Age <- as.factor(df$Age)

# PCSI + EMP
metier_cat <- c("Agriculteurs", "Indépendants", "Cadres sups", "Prof. intermédiaires",
                "Employés", "Ouvriers", "Retraités", "Inactifs")
df$PCSI <- mgsub(df$PCSI, pattern = 1:8, replacement = metier_cat)
df$PCSI[df$PCSI == "Inactifs" & df$Emp == 5] <- "Sans Emploi"
df$PCSI[df$PCSI == "Inactifs" & df$Emp == 6] <- "H/F au Foyer"
df$PCSI[df$PCSI == "Inactifs" & df$Emp == 7] <- "Retraités"
df$PCSI[df$PCSI == "Inactifs" & df$Emp == 8] <- "Etudiants"
df$PCSI <- relevel(as.factor(df$PCSI), ref = "Cadres sups")

# Variable HorsDomicile
df$HorsDomicile <- NA
df$HorsDomicile[which(!is.na(df$C6_0))] <- 0
df$HorsDomicile[df$PCSI == "Sans Emploi" | df$PCSI == "H/F au Foyer" 
                | df$PCSI == "Etudiants" | df$PCSI == "Retraités"] <- 0
df$HorsDomicile[which(df$C6_0 == 1 | df$C6_1 == 1 | df$C6_2 == 1 
                      | df$C6_3 == 1)] <- 1
df$HorsDomicile <- as.factor(df$HorsDomicile)

# Variable ProfSante
df$ProfSante <- NA
df$ProfSante[which(!is.na(df$C6_0))] <- "Non"
df$ProfSante[df$PCSI == "Sans Emploi" | df$PCSI == "H/F au Foyer" 
             | df$PCSI == "Etudiants" | df$PCSI == "Retraités"] <- "Non"
df$ProfSante[which(df$C6_0 == 8 | df$C6_1 == 8 | df$C6_2 == 8 
                   | df$C6_3 == 8)] <- "Oui"
df$ProfSante <- as.factor(df$ProfSante)

# Variable Diplome
df$Diplome <- NA
df$Diplome[which(df$C10 == 1)] <- "Sans Diplome"
df$Diplome[which(df$C10 == 2 | df$C10 == 3)] <- "- de BAC"
df$Diplome[which(df$C10 >= 4 & df$C10 <= 5)] <- "BAC à BAC+2"
df$Diplome[which(df$C10 == 6)] <- "BAC+3 ou +"
df$Diplome[which(df$C10 == 7)] <- NA
df$Diplome <- as.factor(df$Diplome)
df$Diplome <- relevel(as.factor(df$Diplome), ref = "Sans Diplome")
df$Diplome <- factor(df$Diplome, levels = c("Sans Diplome", "- de BAC", 
                                            "BAC à BAC+2", "BAC+3 ou +"))


# Variable Infecte
df$Infecte <- NA
df$Infecte[which(df$A1 == 2)] <- 0
df$Infecte[which(df$A2 < 3)] <- 1
df$Infecte[which(df$A2 > 2 && df$A2 == 7)] <- 0
df$Infecte <- as.factor(df$Infecte)

reg <- glm(formula = Infecte ~ PCSI + HorsDomicile + Sexe + Age + Diplome 
           + ProfSante, family = binomial(link = logit), data = df)
summary(reg)

tbl_regression(reg, exponentiate = TRUE)

# Figure 25 : Nombre et fréquence des infectés dans notre échantillon
table_Infecte <- t(data.frame(
  Nombre = as.character(unclass(table(df$Infecte))),
  Frequence = paste(as.character(round(unclass(table(df$Infecte)) 
                                       / sum(!is.na(df$Infecte)) * 100, 3)), "%")
))
colnames(table_Infecte) <- c("N'a pas contracté le virus", "A contracté le virus")

View(table_Infecte)
