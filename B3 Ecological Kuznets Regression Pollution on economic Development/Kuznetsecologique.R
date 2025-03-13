# Charger les packages nécessaires
install.packages("readxl")  # Installer le package si nécessaire
library(readxl)

# Définir le répertoire de travail
setwd("D:/Documents/Bibliothèque/1.1 econometrie/DONNEES AVEC R")

# Lire le fichier Excel
ODD_REG <- read_excel("ODD_REG.xlsx")

# Créer un vecteur de noms de régions
b <- c("Alsace", "Ile de France", "Midi Pyrénées", "Aquitaine", "Poitou", "Rhone Alpes", 
       "Centre, Auvergne", "Poitou Charentes", "Franche Comte", "Bourgogne", 
       "Languedoc-Roussillon", "Basse-Normandie", "Corse", "Haute-Normandie", 
       "Picardie", "Limousin", "Nord pas de Calais", "Pays de la Loire")

# Créer un vecteur de valeurs associées
c <- c(842, 800, 795, 801, 773, 825, 806, 774, 790, 747, 770, 796, 755, 796, 779, 775, 728, 788)

# Créer un dataframe
if (length(b) == length(c)) {
  df <- data.frame(Region = b, Value = c)
} else {
  stop("Les vecteurs b et c doivent avoir la même longueur")
}

# Créer un vecteur PIB
PIB <- c(842, 800, 795, 801, 773, 825, 806, 774, 790, 747, 770, 796, 755, 796, 779, 775, 728, 788)

# Créer un vecteur Indice Enviroscope
IndiceEnviroscopeX10 <- c(103, 88, 84, 81, 76, 75, 74, 72, 68, 66, 60, 58, 52)

# Créer un vecteur IDH2
IDH2 <- c(765, 780, 785, 800, 775, 810, 790, 795, 800, 780, 710, 765, 700)

# Vérifier la cohérence des longueurs
min_length <- min(length(IDH2), length(IndiceEnviroscopeX10))
IDH2 <- IDH2[1:min_length]
IndiceEnviroscopeX10 <- IndiceEnviroscopeX10[1:min_length]

# Créer un dataframe pour IDH et Indice Enviroscope
df_indices <- data.frame(IDH2 = IDH2, IndiceEnviroscopeX10 = IndiceEnviroscopeX10)

# Effectuer une régression linéaire
reg1 <- lm(IndiceEnviroscopeX10 ~ IDH2)
summary(reg1)

# Tracer le graphique
plot(IDH2, IndiceEnviroscopeX10, main = "Relation entre IDH et Indice Enviroscope", 
     xlab = "IDH2", ylab = "Indice Enviroscope X10", pch = 16, col = "blue")
abline(reg1, col = "red", lwd = 2)
