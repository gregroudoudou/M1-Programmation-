# Chemin vers le fichier de données
file_path <- "C:/Users/grego/OneDrive/Documents/sampled_data1.csv"
output_path <- "C:/Users/grego/OneDrive/Documents/pooled_model_summary.txt"
library(readr)
library(data.table)




# Charger les bibliothèques nécessaires
library(plm)
library(dplyr)
library(readr)

# Charger les données
cat("Chargement des données...\n")
data <- read_csv(file_path)


##Parties descriptives. 



cat("Aperçu des données avant traitement :\n")
print(head(data))

# Filtrer les variables explicatives X
x_variables <- c("VEGMO", "ORANGYR", "FRUITMO", "BREAKFAST", 
                 "SNACKS", "HRSLEEP", "VIG10DMIN", "EDUC")
if (!all(x_variables %in% colnames(data))) {
  stop("Certaines variables explicatives manquent dans les données.")
}
y_variable <-c("EARNIMP4")



library("dplyr")
#Transformer notamment pour la variable FRUTNO 
library(dplyr)

# Remplacer les valeurs spécifiques par NA
library(dplyr)

# Remplacer les valeurs 996, 997, 998, 999 par NA dans frutno et vegeno
data <- data %>%
  mutate(across(c(FRUTNO, VEGENO), ~ ifelse(. %in% c(996, 997, 998, 999), NA, .)))

# Vérifier les changements
table(data$FRUTNO, useNA = "always")  # Vérifier pour frutno
table(data$VEGENO, useNA = "always")  # Vérifier pour vegeno


###Pour VIG10DMIN : 
library(dplyr)

# Remplacer les valeurs spécifiques par NA dans vig10
data <- data %>%
  mutate(VIG10DMIN = ifelse(VIG10DMIN %in% c(997, 998, 999), NA, VIG10DMIN))

# Vérifier les changements
table(data$VIG10DMIN, useNA = "always")  # Vérifier si les NA sont bien intégrés

###Point sur le nombre de valeurs manquantes par données. 
colSums(is.na(data))
nrow(data)

# Calculer le pourcentage de valeurs manquantes par colonne
missing_percent <- colSums(is.na(data)) / nrow(data) * 100

# Afficher les colonnes triées par pourcentage de valeurs manquantes
sort(missing_percent, decreasing = TRUE)


####Transformer la variable EDUCation ici en années : 
library(dplyr)

library(dplyr)

# Recodification des codes d'éducation en années de scolarité (+1 année)
data <- data %>%
  mutate(EDUC = case_when(
    EDUC == 102 ~ "Maternelle",  # Kindergarten seulement -> 1 an
    EDUC == 104 ~ "CP",  # Grade 1 -> 1+1 = 2 ans
    EDUC == 105 ~ "CE1",  # Grade 2 -> 2+1 = 3 ans
    EDUC == 106 ~ "CE2",  # Grade 3 -> 3+1 = 4 ans
    EDUC == 107 ~ "CM1",  # Grade 4 -> 4+1 = 5 ans
    EDUC == 108 ~ "CM2",  # Grade 5 -> 5+1 = 6 ans
    EDUC == 109 ~ "6eme",  # Grade 6 -> 6+1 = 7 ans
    EDUC == 110 ~ "5eme",  # Grade 7 -> 7+1 = 8 ans
    EDUC == 111 ~ "4eme",  # Grade 8 -> 8+1 = 9 ans
    EDUC == 113 ~ "3eme", # Grade 9 -> 9+1 = 10 ans
    EDUC == 114 ~ "Seconde", # Grade 10 -> 10+1 = 11 ans
    EDUC == 115 ~ "Premiere", # Grade 11 -> 11+1 = 12 ans
    EDUC == 116 ~ "Bacrate", # Grade 12 (pas de diplôme) -> 12+1 = 13 ans
    EDUC == 200 ~ "Lycee", # Diplôme de secondaire (High school) -> 12+1 = 13 ans
    EDUC == 201 ~ "Lyceeadulte", # Diplômé du secondaire -> 12+1 = 13 ans
    EDUC == 202 ~ "13", # GED ou équivalent -> 12+1 = 13 ans
    EDUC == 300 ~ "1ereanneeuniv", # 1ère année d'université -> 13+1 = 14 ans
    EDUC == 301 ~ "14", # 1-3 ans d'université -> 13+1 = 14 ans
    EDUC == 302 ~ "Bac2technique", # Diplôme technique (AA degree) -> 14+1 = 15 ans
    EDUC == 303 ~ "Bac2univ", # Diplôme académique (AA degree) -> 14+1 = 15 ans
    EDUC == 400 ~ "Bachelor", # Licence (Bachelor's degree) -> 16+1 = 17 ans
    EDUC == 501 ~ "Master", # Master -> 18+1 = 19 ans
    EDUC == 502 ~ "EcolePro", # École professionnelle (MD, JD, etc.) -> 20+1 = 21 ans
    EDUC == 503 ~ "Doctorat", # Doctorat (PhD, EdD) -> 20+1 = 21 ans
    EDUC == 505 ~ "Doctoratpro", # Doctorat/École professionnelle (top) -> 20+1 = 21 ans
    EDUC %in% c(996, 997, 998, 999) ~ NA_character_, # Cas inconnus -> NA
    TRUE ~ NA_character_ # Autres cas inconnus -> NA
  ))
colnames(data)  # Liste toutes les colonnes générées

# Affichage du tableau mis à jour
print(data)




###Traitemetn de  l'éducation en dummies 
library(dplyr)
library(fastDummies)

data <- data %>%
  mutate(EDUC = factor(EDUC)) %>%  # Convertir en facteur si ce n'est pas déjà fait
  dummy_cols(select_columns = "EDUC", remove_selected_columns = FALSE)

###Ainsi la liste des dummys est la suivante , en enlevant la maternelle. 
# EDUC_CP + EDUC_CE1 + EDUC_CE2 + EDUC_CM1 + EDUC_CM2 + EDUC_6eme + EDUC_5eme + EDUC_4eme + EDUC_3eme + EDUC_Seconde + EDUC_Premiere + EDUC_Bacraté + EDUC_Lycée + EDUC_Lycéeadulte + EDUC_13 + EDUC_1ereannéeuniv + EDUC_14 + EDUC_Bac+2technique + EDUC_Bac+2univ + EDUC_Bachelor + EDUC_Master + EDUC_EcolePro + EDUC_Doctorat + EDUC_Doctoratpro


###Transformation de sexe en dummies 

###DUmmys pour sexe

# Installer et charger le package
library(fastDummies)

# Supposons que votre data frame s'appelle 'data'
# Créer des variables dummy pour la colonne 'sex'
data <- dummy_cols(data, select_columns = "SEX", remove_first_dummy = TRUE)


####Traitement de EARNIMP4 et notamment sa transformation en variable continue. 

library(dplyr)


# Recodification en valeur continue (valeur médiane de chaque tranche)
data <- data %>%
  mutate(EARNIMP4 = case_when(
    EARNIMP4 == 1  ~ 2500,   # $1 - $4,999 -> Médiane = $2,500
    EARNIMP4 == 2  ~ 7500,   # $5,000 - $9,999 -> Médiane = $7,500
    EARNIMP4 == 3  ~ 12500,  # $10,000 - $14,999 -> Médiane = $12,500
    EARNIMP4 == 4  ~ 17500,  # $15,000 - $19,999 -> Médiane = $17,500
    EARNIMP4 == 5  ~ 22500,  # $20,000 - $24,999 -> Médiane = $22,500
    EARNIMP4 == 10 ~ 30000,  # $25,000 - $34,999 -> Médiane = $30,000
    EARNIMP4 == 11 ~ 27500,  # $25,000 - $29,999 -> Médiane = $27,500
    EARNIMP4 == 12 ~ 32500,  # $30,000 - $34,999 -> Médiane = $32,500
    EARNIMP4 == 20 ~ 40000,  # $35,000 - $44,999 -> Médiane = $40,000
    EARNIMP4 == 21 ~ 37500,  # $35,000 - $39,999 -> Médiane = $37,500
    EARNIMP4 == 22 ~ 42500,  # $40,000 - $44,999 -> Médiane = $42,500
    EARNIMP4 == 30 ~ 50000,  # $45,000 - $54,999 -> Médiane = $50,000
    EARNIMP4 == 31 ~ 47500,  # $45,000 - $49,999 -> Médiane = $47,500
    EARNIMP4 == 32 ~ 52500,  # $50,000 - $54,999 -> Médiane = $52,500
    EARNIMP4 == 40 ~ 60000,  # $55,000 - $64,999 -> Médiane = $60,000
    EARNIMP4 == 41 ~ 57500,  # $55,000 - $59,999 -> Médiane = $57,500
    EARNIMP4 == 42 ~ 62500,  # $60,000 - $64,999 -> Médiane = $62,500
    EARNIMP4 == 50 ~ 70000,  # $65,000 - $74,999 -> Médiane = $70,000
    EARNIMP4 == 51 ~ 67500,  # $65,000 - $69,999 -> Médiane = $67,500
    EARNIMP4 == 52 ~ 72500,  # $70,000 - $74,999 -> Médiane = $72,500
    EARNIMP4 == 60 ~ 85000,  # $75,000 et plus -> Estimé à $85,000
    EARNIMP4 == 61 ~ 77500,  # $75,000 - $79,999 -> Médiane = $77,500
    EARNIMP4 == 62 ~ 82500,  # $80,000 - $84,999 -> Médiane = $82,500
    EARNIMP4 == 63 ~ 87500,  # $85,000 - $89,999 -> Médiane = $87,500
    EARNIMP4 == 64 ~ 92500,  # $90,000 - $94,999 -> Médiane = $92,500
    EARNIMP4 == 65 ~ 97500,  # $95,000 - $99,999 -> Médiane = $97,500
    EARNIMP4 == 66 ~ 110000, # $100,000 et plus -> Estimé à $110,000
    EARNIMP4 == 67 ~ 102500, # $100,000 - $104,999 -> Médiane = $102,500
    EARNIMP4 == 68 ~ 107500, # $105,000 - $109,999 -> Médiane = $107,500
    EARNIMP4 == 69 ~ 112500, # $110,000 - $114,999 -> Médiane = $112,500
    EARNIMP4 == 70 ~ 120000, # $115,000 et plus -> Estimé à $120,000
    EARNIMP4 %in% c(97, 98, 99) ~ NA_real_,  # Valeurs inconnues
    TRUE ~ NA_real_
  ))

# Affichage du dataset recodé
print(data)







###Traitement des valeurs manquantes 

# Remplacement des valeurs manquantes par la médiane pour les colonnes numériques
cat("Remplacement des valeurs manquantes par la médiane...\n")
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Convertir SERIAL et YEAR en facteurs
data <- data %>%
  mutate(SERIAL = as.factor(SERIAL),
         YEAR = as.factor(YEAR)) 
# Identifier et gérer les duplications
cat("Recherche des duplications...\n")
duplicates <- data %>%
  group_by(SERIAL, YEAR) %>%
  filter(n() > 1)

if (nrow(duplicates) > 0) {
  cat("Doublons détectés dans SERIAL et YEAR. Voici un aperçu :\n")
  print(duplicates)
  
  # Supprimer les duplications en gardant la première occurrence
  data <- data %>%
    distinct(SERIAL, YEAR, .keep_all = TRUE)
  cat("Les duplications ont été supprimées.\n")
} else {
  cat("Aucune duplication détectée.\n")
}


###Partie éducation en dummies. 

summary(data)


###Partie normalisation pour data 1

# Normalisation (Z-score) de Y et des X
cat("Normalisation des données (Z-score)...\n")
data1 <- data %>%
  mutate(
    across(all_of(c(y_variable, x_variables)), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
  )



###GENERALE pour la formule : arrêt ici : avec création panel et formule réutilisable

# Créer un DataFrame panel (indexé par SERIAL et YEAR)
pdata <- pdata.frame(data, index = c("SERIAL", "YEAR"))

# Formule de régression EDUC 6 ENLEVE 
formula <- log(EARNIMP4) ~ VEGENO + ORANGYR + FRUTNO + BREAKFAST + SNACKS + HRSLEEP + VIG10DMIN + AGE + I(AGE^2) + SEX + EDUC_13 + EDUC_14 + EDUC_3eme + EDUC_4eme + EDUC_5eme  + EDUC_Bac2technique + EDUC_Bac2univ + EDUC_Bachelor + EDUC_Bacrate + EDUC_CE1 + EDUC_CE2 + EDUC_CM1 + EDUC_CM2 + EDUC_CP + EDUC_Doctorat + EDUC_Doctoratpro + EDUC_EcolePro + EDUC_Lyceeadulte + EDUC_Master + EDUC_Maternelle + EDUC_Premiere + EDUC_Seconde 

###

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_model <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})


library(plm)       # Modèles de panel (Between, Within, Random)
library(stargazer) # Résumé des modèles en tableau
library(car)       # Calcul du VIF
library(nlme)      # Modèle FE-GLS et FGLS


# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_model))

#Tentons avec la première imputation du salaire et voyons si les résultats s'améliorent. 

# Formule de régression
formula <- EARNIMP4 ~ VEGNO + ORANGYR + FRUTNO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_model <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_model))


#2eme EARNIMP : 

# Formule de régression
formula <- EARNIMP1 ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_model <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_model))
#2eme earnimp : 2eme type d'imputations manquantes 


# Formule de régression
formula <- EARNIMP2 ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_modelIMP2 <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelIMP2))

#3eme earnimp : 
# Formule de régression
formula <- EARNIMP3 ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_modelIMP3 <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelIMP3))

#4eme méthode de EARNIMP4 : 


# Formule de régression
formula <- EARNIMP4 ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_modelIMP4 <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelIMP4))


# Formule de régression
formula <- EARNIMP5 ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS...\n")
pooled_modelIMP5 <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelIMP5))

stargazer(pooled_model,pooled_modelIMP2,pooled_modelIMP3,pooled_modelIMP4,pooled_modelIMP5)

export_results <- function(model, output_path) {
  # Exporter les résultats dans un fichier
  if (!is.null(model)) {
    tryCatch({
      summary_text <- capture.output(summary(model))
      writeLines(summary_text, con = output_path)
      cat(sprintf("\nRésumé exporté avec succès vers %s\n", output_path))
    }, error = function(e) {
      cat(sprintf("Erreur lors de l'exportation : %s\n", e$message))
    })
  } else {
    cat("Aucun modèle à exporter.\n")
  }
}

# Exécution principale
pooled_model <- run_log_model(file_path)
export_results(pooled_model, output_path)

#####Nouveuax modeles : 
# Charger le package nécessaire
library(nlme)

# Ajuster le modèle GLS
gls_model <- tryCatch({
  gls(formula, data = pdata, method = "REML")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle GLS :", e$message)
})

# Résumé du modèle GLS
summary(gls_model)

###Nouveau modele GLS : 
# Charger le package nécessaire
library(nlme)

# Ajuster le modèle GLS avec structure de corrélation et hétéroscédasticité
gls_model <- tryCatch({
  gls(formula, data = pdata, 
      correlation = corAR1(form = ~ 1 | SERIAL),  # Structure de corrélation AR(1) pour les individus # Gestion de l'hétéroscédasticité par groupe
      method = "REML")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle GLS :", e$message)
})

# Résumé du modèle GLS
summary(gls_model)

# Calcul du pseudo-R² de Nagelkerke (variance expliquée)
R2_gls <- 1 - (gls_model$sigma^2 / var(pdata$EARNIMP4))

cat("Pseudo-R² de Nagelkerke:", R2_gls, "\n")



####Utiliser les valeurs imputées pour les revenus personnels : un changement dans le R2 ? 

# Convertir SERIAL et YEAR en facteurs
data <- data %>%
  mutate(SERIAL = as.factor(SERIAL),
         YEAR = as.factor(YEAR))


# Variables explicatives
x_variables <- c("VEGMO", "ORANGYR", "FRUITMO", "BREAKFAST", 
                 "SNACKS", "HRSLEEP", "VIG10DMIN", "EDUC", "AGE", "SEX")

# Variables dépendantes à tester
earnimp_vars <- c("EARNIMP1", "EARNIMP2", "EARNIMP3", "EARNIMP4", "EARNIMP5")

# Liste pour stocker les modèles
models <- list()

# Ajuster un modèle pour chaque variable dépendante
for (var in earnimp_vars) {
  # Construire la formule de régression
  formula <- as.formula(paste(var, "~", paste(x_variables, collapse = " + ")))
  
  # Ajustement du modèle Pooled OLS
  cat(sprintf("Ajustement du modèle Pooled OLS pour la variable %s...\n", var))
  pooled_model <- tryCatch({
    plm(formula, data = pdata, model = "pooling")
  }, error = function(e) {
    stop(sprintf("Erreur lors de l'ajustement du modèle pour %s : %s", var, e$message))
  })
  
  # Stocker le modèle
  models[[var]] <- pooled_model
}

# Exporter les résultats avec stargazer
jpeg("C:/Users/grego/OneDrive/Documents/pooled_model_results.jpeg", width = 1000, height = 800)
stargazer(models, type = "text", title = "Résultats des modèles Pooled OLS", align = TRUE)
dev.off()

cat("\nLes résultats ont été exportés au format JPEG dans le fichier spécifié.\n")

###Régression sur le EARNIMP4 avec le logarithme. 


####Cherchons le meilleur modèle l'exhaustive search. 

# Charger les bibliothèques nécessaires
library(plm)
library(dplyr)
library(readr)
library(leaps)
library(stargazer)


# Variables explicatives (incluant AGE et SEX)
x_variables <- c("VEGMO", "ORANGYR", "FRUITMO", "BREAKFAST", 
                 "SNACKS", "HRSLEEP", "VIG10DMIN", "EDUC", "AGE", "SEX")

# Variable dépendante
y_variable <- "EARNIMP4"



# Préparer les données pour la sélection exhaustive
temp_data <- data %>% select(all_of(c(y_variable, x_variables))) %>% na.omit()

# Effectuer la sélection exhaustive
exhaustive_search <- regsubsets(as.formula(paste(y_variable, "~ .")), 
                                data = temp_data, 
                                nvmax = length(x_variables))

# Résumé des modèles ajustés
model_summary <- summary(exhaustive_search)

# Trouver le modèle avec le meilleur R² ajusté
best_model_index <- which.max(model_summary$adjr2)

# Extraire les variables sélectionnées
best_variables <- names(which(model_summary$which[best_model_index, -1]))

# Construire la formule avec les meilleures variables
best_formula <- as.formula(paste(y_variable, "~", paste(best_variables, collapse = " + ")))

# Ajuster le modèle Pooled OLS avec les meilleures variables
cat("Ajustement du modèle Pooled OLS avec les meilleures variables...\n")
best_pooled_model <- plm(best_formula, data = pdata, model = "pooling")

# Résultat final
cat("Résumé du modèle sélectionné :\n")
print(summary(best_pooled_model))

# Exporter les résultats avec stargazer
jpeg("C:/Users/grego/OneDrive/Documents/pooled_model_best_results.jpeg", width = 1000, height = 800)
stargazer(best_pooled_model, type = "text", title = "Meilleur modèle Pooled OLS avec EARNIMP4")
dev.off()

cat("\nLes résultats du meilleur modèle ont été exportés au format JPEG.")

#Ainsi, si l'on souhaite le modèle avec le meilleur R2, il faudrait toutefois supprimer la variable VEGENO. 


#Créons la transformation de rêve : le logarithme. 

# Filtrer les observations avec EARNIMP4 > 0
pdata <- pdata[pdata$EARNIMP4 > 0 & !is.na(pdata$EARNIMP4), ]

# Formule de régression (avec VEGMO et log(EARNIMP4))
formula <- log(EARNIMP4) ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS avec log(EARNIMP4)...\n")
pooled_modelIMP4 <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelIMP4))

#En comparant les modèles, la suppression de VEGMO, fait diminuer la valeur d'impact
#de manger un fruit de 3%. Ainsi, on pouvait souligner un problème de variable omise. 
#Cela pourrait s'expliquer par l'abus de sucre potentiellement. 
#Augmenter de 1 unité dans FRUITMO, correspond à environ 30 fruits de plus dans l'année. 
#Ainsi, en moyenne, cela diminuerait de 10% le salaire. 
#Fruitmo , en général, en lien potentiellement avec le sucre, pourrait être



#On va donc ajouter la variable SWEET afin d'avoir plus de recul. sur notre analyse. 


# Formule de régression (sans VEGMO et log(EARNIMP4))
formula <- log(EARNIMP4) ~  + ORANGYR + FRUITMO + BREAKFAST + 
  SNACKS + HRSLEEP + VIG10DMIN + EDUC

# Ajuster le modèle Pooled OLS
cat("Ajustement du modèle Pooled OLS avec log(EARNIMP4)...\n")
pooled_modelLIN <- tryCatch({
  plm(formula, data = pdata, model = "pooling")
}, error = function(e) {
  stop("Erreur lors de l'ajustement du modèle :", e$message)
})

# Résultat final
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelLIN))


####7  : comparer entre tous les modèles de panel 


# Définition de la formule initiale
formula <- EARNIMP4 ~ BREAKFAST + HRSLEEP + EDUC + MOD10DMIN + VEGENO + SEX + I(AGE^2) + AGE 

# Fonction pour ajuster le modèle et vérifier le VIF
reduce_multicollinearity <- function(data, formula, vif_threshold = 10) {
  
  # Convertir la formule en une liste de variables
  vars <- all.vars(formula)
  dependent_var <- vars[1]  # Variable dépendante
  independent_vars <- vars[-1]  # Variables explicatives
  
  # Boucle pour éliminer les variables à VIF élevé
  while (TRUE) {
    # Ajuster le modèle Between Effects (BE)
    be_model <- plm(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))),
                    data = data, model = "between", effect = "time")
    
    # Calculer le VIF
    vif_values <- vif(be_model)
    
    # Vérifier si tous les VIF sont inférieurs au seuil
    if (all(vif_values < vif_threshold)) {
      cat("\n✅ Toutes les variables ont un VIF < ", vif_threshold, "\n")
      break
    }
    
    # Trouver la variable avec le plus grand VIF
    max_vif_var <- names(vif_values)[which.max(vif_values)]
    cat("\n🚨 Suppression de la variable :", max_vif_var, "avec VIF =", max(vif_values), "\n")
    
    # Supprimer cette variable de la liste
    independent_vars <- setdiff(independent_vars, max_vif_var)
    
    # Vérifier qu'il reste au moins une variable explicative
    if (length(independent_vars) == 0) {
      cat("\n❌ Plus de variables explicatives disponibles ! Arrêt.\n")
      break
    }
  }
  
  # Retourner la nouvelle formule optimisée
  return(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))))
}

# Exécuter la fonction sur les données pdata
optimized_formula <- reduce_multicollinearity(pdata, formula)

# Afficher la formule finale après réduction de la multicolinéarité
cat("\n🔹 Nouvelle formule optimisée :", deparse(optimized_formula), "\n")


# Vérification des données panel
library(plm)
if (!"pdata.frame" %in% class(pdata)) {
  pdata <- pdata.frame(pdata, index = c("serial", "year"))  # Remplacez "id" et "time" par vos colonnes d'index
}

####2eme vérification
# Test d'alias (multicolinéarité parfaite)
alias(lm(EARNIMP4 ~ BREAKFAST + HRSLEEP + EDUC + MOD10DMIN + VEGENO + SEX + I(AGE^2) + AGE, data = pdata))

###2eme test :
# Sélection des variables explicatives sous forme de matrice
X <- model.matrix(~ BREAKFAST + HRSLEEP + EDUC + MOD10DMIN + VEGENO + SEX + I(AGE^2) + AGE, data = pdata)

# Décomposition en valeurs singulières
svd_decomp <- svd(X)

# Affichage des valeurs singulières
print(svd_decomp$d)

# Vérification de la condition number (plus le ratio max/min est grand, plus il y a de multicolinéarité)
condition_number <- max(svd_decomp$d) / min(svd_decomp$d)
print(condition_number)



### Partie RESTE !


# Formule de régression EDUC 6 ENLEVE 
formula <- log(EARNIMP4) ~ VEGENO + ORANGYR  + BREAKFAST + HRSLEEP + VIG10DMIN + AGE + I(AGE^2) + SEX + EDUC_13 + EDUC_14 + EDUC_3eme + EDUC_4eme + EDUC_5eme  + EDUC_Bac2technique + EDUC_Bac2univ + EDUC_Bachelor + EDUC_Bacrate + EDUC_CE1 + EDUC_CE2 + EDUC_CM1 + EDUC_CM2 + EDUC_CP + EDUC_Doctorat + EDUC_Doctoratpro + EDUC_EcolePro + EDUC_Lyceeadulte + EDUC_Master + EDUC_Maternelle + EDUC_Premiere + EDUC_Seconde 



# Pooled OLS
cat("Ajustement du modèle Pooled OLS avec log(EARNIMP4)...\n")
pooled_modelLIN <- plm(formula, data = pdata, model = "pooling")
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelLIN))




# Fixed Effects (FE)
cat("\nAjustement du modèle Fixed Effects (FE) avec log(EARNIMP4)...\n")
fe_modelLIN <- plm(formula, data = pdata, model = "within",effect="twoways")
summary(fe_modelLIN)
cat("Résumé du modèle Fixed Effects :\n")
coeftest(fe_modelLIN, vcov = vcovHC(fe_modelLIN, type = "HC0", cluster = "group"))

print(summary(fe_modelLIN))
pFtest(fe_modelLIN,pooled_modelLIN)
bptest(fe_modelLIN)
library(stargazer)
stargazer(pooled_modelLIN,fe_modelLIN,be_modelTIME,re_modelLIN,type = "text", title = "Modèles de Panel")


library(sandwich)
coeftest(fe_modelLIN, vcov = vcovHAC(fe_modelLIN))



cat("\nAjustement du modèle Fixed Effects (FE) avec log(EARNIMP4)...\n")
fe_modelLIN <- plm(formula, data = pdata, model = "within", effect="time")
cat("Résumé du modèle Fixed Effects :\n")
print(summary(fe_modelLIN))

pFtest(fe_modelLIN,pooled_modelLIN)

#Effect individuel
cat("\nAjustement du modèle Fixed Effects (FE) avec log(EARNIMP4)...\n")
fe_modelLIN <- plm(formula, data = pdata, model = "within", effect="twoways")
cat("Résumé du modèle Fixed Effects :\n")
print(summary(fe_modelLIN))
library(sandwich)
library(lmtest)
coeftest(fe_modelLIN, vcov = vcovHC(fe_modelLIN, type = "HC0"))
coeftest(fe_modelLIN, vcov = vcovHC(fe_modelLIN, type = "HC0", cluster = "time"))

pFtest(fe_modelLIN,pooled_modelLIN)


phtest(fe_modelLIN, be_modelLIN)
# Random Effects (RE)
cat("\nAjustement du modèle Random Effects (RE) avec log(EARNIMP4)...\n")
re_modelLIN <- plm(formula, data = pdata, model = "random")
cat("Résumé du modèle Random Effects :\n")
print(summary(re_modelLIN))

# Random Effects (RE)
cat("\nAjustement du modèle Random Effects (RE) avec log(EARNIMP4)...\n")
re_modelLIN <- plm(formula, data = pdata, model = "random",effect="time")
cat("Résumé du modèle Random Effects :\n")
print(summary(re_modelLIN))

vif2<-vif(re_modelLIN)
print(vif2)
cat("\nAjustement du modèle Random Effects (RE) avec log(EARNIMP4)...\n")
re_indiv <- plm(formula, data = pdata, model = "random",effect="indiv")
cat("Résumé du modèle Random Effects :\n")
print(summary(re_indiv))


cat("\nAjustement du modèle Random Effects (RE) avec log(EARNIMP4)...\n")
re_randomtwo <- plm(formula, data = pdata, model = "random",effect="twoways")
cat("Résumé du modèle Random Effects :\n")
print(summary(re_randomtwo ))


# Between Effects (BE)
cat("\nAjustement du modèle Between Effects (BE) avec log(EARNIMP4)...\n")
be_modelTIME <- plm(formula, data = pdata, model = "between",effect="time")
cat("Résumé du modèle Between Effects :\n")
print(summary(be_modelTIME))


bptest(be_modelTIME)
bptest(fe_modelLIN)
# Between Effects (BE)
cat("\nAjustement du modèle Between Effects (BE) avec log(EARNIMP4)...\n")
be_modelINDIV <- plm(formula, data = pdata, model = "between",effect="indiv")
cat("Résumé du modèle Between Effects :\n")
print(summary(be_modelINDIV))

# Pour le modèle avec effet temporel
coeftest_be_modelTIME_robust <- coeftest(be_modelTIME, vcov = vcovHC(be_modelTIME, method = "arellano", type = "HC0"))
print(coeftest_be_modelTIME_robust)

# Pour le modèle avec effet individuel
coeftest_be_modelINDIV_robust <- coeftest(be_modelINDIV, vcov = vcovHC(be_modelINDIV, method = "arellano", type = "HC0"))
print(coeftest_be_modelINDIV_robust)



##tEST  de HAUSMAN : 
phtest(re_m)

##test
library(lmtest)
pFtest(fe_modelLIN, test="Chisq")


phtest(fe_modelLIN, re_modelLIN)

library(car)
vif1<-vif(be_modelLIN)
print(vif1)

library(lmtest)
coeftest(be_modelLIN, vcov = vcovHC(be_modelLIN, type = "HC0", cluster = "group"))

###Faire le test de Mundlak
# Charger les packages nécessaires
library(plm)


# Créer les moyennes individuelles des variables explicatives
df_panel$VEGENO_mean <- ave(df_panel$VEGENO, df_panel$id, FUN=mean)
df_panel$ORANGYR_mean <- ave(df_panel$ORANGYR, df_panel$id, FUN=mean)
df_panel$FRUTNO_mean <- ave(df_panel$FRUTNO, df_panel$id, FUN=mean)
df_panel$BREAKFAST_mean <- ave(df_panel$BREAKFAST, df_panel$id, FUN=mean)
df_panel$HRSLEEP_mean <- ave(df_panel$HRSLEEP, df_panel$id, FUN=mean)
df_panel$VIG10DMIN_mean <- ave(df_panel$VIG10DMIN, df_panel$id, FUN=mean)
df_panel$AGE_mean <- ave(df_panel$AGE, df_panel$id, FUN=mean)
df_panel$SEX_mean <- ave(df_panel$SEX, df_panel$id, FUN=mean)

# Appliquer la régression Mundlak avec le modèle à effets aléatoires
formula_mundlak <- log(EARNIMP4) ~ VEGENO + ORANGYR + FRUTNO + BREAKFAST + HRSLEEP + 
  VIG10DMIN + AGE + I(AGE^2) + SEX +
  EDUC_13 + EDUC_14 + EDUC_3eme + EDUC_4eme + EDUC_5eme  + 
  EDUC_Bac2technique + EDUC_Bac2univ + EDUC_Bachelor + EDUC_Bacrate + 
  EDUC_CE1 + EDUC_CE2 + EDUC_CM1 + EDUC_CM2 + EDUC_CP + EDUC_Doctorat + 
  EDUC_Doctoratpro + EDUC_EcolePro + EDUC_Lyceeadulte + EDUC_Master + 
  EDUC_Maternelle + EDUC_Premiere + EDUC_Seconde +
  VEGENO_mean + ORANGYR_mean + FRUTNO_mean + BREAKFAST_mean + 
  HRSLEEP_mean + VIG10DMIN_mean + AGE_mean + SEX_mean

model_mundlak <- plm(formula_mundlak, data = df_panel, model = "random")

# Résumé des résultats
summary(model_mundlak)




cat("\nAjustement du modèle Between Effects (BE) avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between",effect="indiv")
cat("Résumé du modèle Between Effects :\n")
print(summary(be_modelLIN))


cat("\nAjustement du modèle Between Effects (BE) avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between",effect="twoways")
cat("Résumé du modèle Between Effects :\n")
print(summary(be_modelLIN))





library("lmtest")
bptest(be_modelLIN)

#multicolinéarité. 
library(car)
vif(be_modelLIN)


library(nlme)
###Faire un gls pour résoudre le problème d'hétéroscédasticité

# Charger les packages nécessaires
library(nlme)
library(dplyr)

# Vérifier si SERIAL est bien défini comme un facteur
pdata <- pdata %>%
  mutate(SERIAL = as.factor(SERIAL))  # Si SERIAL est un identifiant de groupe

# Ajustement du modèle GLS
gls_model <- gls(
  EARNIMP4 ~ BREAKFAST + HRSLEEP + EDUC + MOD10DMIN + VEGENO + SEX + I(AGE^2) + AGE, 
  data = pdata,
  correlation = corAR1(form = ~1 | SERIAL),  # Structure de corrélation temporelle
  weights = varIdent(form = ~1 | SERIAL)     # Hétéroscédasticité par groupe SERIAL
)

# Résumé du modèle
summary(gls_model)







#on la remplacé par MOD10DMIN


library(sandwich)
library(lmtest)
# Erreurs robustes de type White
coeftest(be_modelLIN, vcov = vcovHC(be_modelLIN, type = "HC0"))


library(nlme)

# Ajustement du modèle GLS
gls_model <- gls(
  formula = formula, 
  data = pdata, 
  correlation = corAR1(form = ~year),  # Corrélation AR(1) sur les années
  weights = varIdent(form = ~1 | SERIAL) # Variance différente par entreprise
)

# Résumé du modèle
summary(gls_model)



####Tests 


###Derniere regression. remodifier la formula qqpart svp

# Formule de régression (ajout de SEXE et AGE)
formula <- EARNIMP4 ~  FRUTNO + VEGENO + BREAKFAST  + HRSLEEP + VIG10DMIN + EDUC + SEX + AGE

# Vérification des données panel
if (!"pdata.frame" %in% class(pdata)) {
  pdata <- pdata.frame(pdata, index = c("id", "time"))  # Remplacez "id" et "time" par vos colonnes d'index
}

library("plm")

#OLS 
model <- lm(formula, data = pdata)
summary(model)
BIC(model)

# Pooled OLS
cat("Ajustement du modèle Pooled OLS avec log(EARNIMP4)...\n")
pooled_modelLIN <- plm(formula, data = pdata, model = "pooling")
cat("Résumé du modèle Pooled OLS :\n")
print(summary(pooled_modelLIN))

# Fixed Effects (FE)
cat("\nAjustement du modèle Fixed Effects (FE) avec log(EARNIMP4)...\n")
fe_indiv_LOG <- plm(formula, data = pdata, model = "within",effect="indiv")
cat("Résumé du modèle Fixed Effects :\n")
print(summary(fe_indiv_LOG))

library(car)
fixedeffect<-vif(fe_indiv_LOG)


#Effet fixe temporel 
fe_timeLOG <- plm(formula, data = pdata, model = "between", effect = "time")
summary(fe_timeLOG)


#Effet fixe twoways 
fe_twoLOG <- plm(formula, data = pdata, model = "within", effect = "twoways")
summary(fe_timeLOG)

fgls1<-fgls()

# Random Effects (RE)
cat("\nAjustement du modèle Random Effects (RE) avec log(EARNIMP4)...\n")
re_modelLIN <- plm(formula, data = pdata, model = "random")
cat("Résumé du modèle Random Effects :\n")
print(summary(re_modelLIN))
library(stargazer)
stargazer (fe_modelLIN, re_modelLIN)

# Between effects (BE) twoways impossible !
anova(fe_indiv_LOG,be_modelLIN)

#Effet between individuel

library(plm)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between", effect="indiv")

cat("Résumé du modèle Between Effects individuel :\n")
print(summary(be_modelLIN))

#Modele Between temporel 

# Définition de la formule (même que précédemment)
formula <- EARNIMP4 ~ ORANGYR  + BREAKFAST + SNACKS + HRSLEEP +
  VIG10DMIN + EDUC + SEX + AGE +I(AGE^2)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between", effect = "time")
#Model=type de modèle en BETWEEN EFFECT
#Ici R2 de 0,90.  
#Mais significativité. 
cat("Résumé du modèle Between Effects individuel :\n")
print(summary(be_modelLIN))


##GLS: 
library(nlme)
library(plm)

# Modèle GLS Between avec effets fixes temporels
be_gls <- gls(
  formula,  # Effet fixe temporel
  data = pdata,
  effect=time,
  method = "REML"
)

# Affichage des résultats
summary(be_gls)


library(plm)
library(nlme)

# 📌 1. Estimer le modèle Between classique
be_model <- plm(log(EARNIMP4) ~ EDUC_13 + EDUC_14 + EDUC_3eme + EDUC_4eme + EDUC_5eme + EDUC_6eme +
                  EDUC_Bac2technique + EDUC_Bac2univ + EDUC_Bachelor + EDUC_Bacrate + 
                  EDUC_CE1 + EDUC_CE2 + EDUC_CM1 + EDUC_CM2 + EDUC_CP + 
                  EDUC_Doctorat + EDUC_Doctoratpro + EDUC_EcolePro + EDUC_Lyceeadulte + 
                  EDUC_Master + EDUC_Maternelle + EDUC_Premiere + EDUC_Seconde + SEX_2 + 
                  experience + I(experience^2),
                data = pdata, model = "between", effect = "individual")

# 📌 2. Vérifier la structure de l'hétéroscédasticité
library(lmtest)
bptest(be_modelLIN)  # Test de Breusch-Pagan pour l'hétéroscédasticité
library(nlme)
# 📌 3. Appliquer FGLS avec correction de variance
fgls_be <- gls(formula,
               data = pdata, weights = varIdent(form = ~ 1 | SERIAL), method = "ML")

summary(fgls_be)


###Le Between est le modèle le plus performant, avec un R2 ajusté le plus élevé.
#Afin d'éviter de tomber dans le datamining, on va vérifier avec le test d'Hausman 
library(sandwich)
library(lmtest)
coeftest(be_modelLIN, vcov=vcovHC(be_modelLIN),type="HC0")

###Test d'hétéroscédasticité
bptest(be_modelLIN)

#Donc H1 est vérifié, il y a bien hétéroscédasticité dans mon modèle. 
#Ainsi les tests des significativité sont incohérentes. 
model_1_coef <- lmtest::coeftest(be_modelLIN, vcov = sandwich::vcovHC) 

##Quels premiers Tests  de modele 
library(car)
vif<-vif(be_modelLIN)
print(vif)
#On remarque que la variable breakfast est fortement colinéaire à la variable snack.
#on va donc décider 'enlever la variable snack et voir la différence aussi avec breakfast. 








####bonne régression 

#1:effet temporel 
formula <- EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST  + HRSLEEP +
  VIG10DMIN + EDUC + SEX + AGE +I(AGE^2)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between", effect = "time")
#Model=type de modèle en BETWEEN EFFECT
summary(be_modelLIN)

#2 : :effet individuel; 
formula <- EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST  + HRSLEEP +
  VIG10DMIN + EDUC + SEX + AGE +I(AGE^2)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_indivLIN <- plm(formula, data = pdata, model = "between", effect = "indiv")
#Model=type de modèle en BETWEEN EFFECT
summary(be_indivLIN)

vif2<-(be_modelLIN)
print(vif2)

#Pour rappelle, ici on a 

##
model_1_coef <- lmtest::coeftest(be_modelLIN, vcov = sandwich::vcovHC) 


coeftest(be_modelLIN, vcov=vcovHC(be_modelLIN, type="HC1", cluster="group"))
vcov_robuste <- vcovHC(be_modelLIN, type = "HC1")

###Tests liés notamment au panel de Breuschpagan pour l'effet temporel : 
x²

##TEST EFFET FIXE TEMPOREL VS EFFET FIXE INDIVIDUEL : 

pFtest(be_modelLIN, be_indivLIN)
#Test : pour le temps fixé, 
#H0 est rejeté , donc on aura bien l'effet temporel ! 

##TEST  : 
plmtest(pooled_model, type=c("bp"))

##Correction
install.packages("pl")
library(plm)

library(plm)
library(lmtest)
pcse_model <- psce(formula, data = pdata, 
                   groupN = pdata$SERIAL, groupT = pdata$YEAR)
summary(pcse_model)

#Test d'autocorrélation
pwartest(be_modelLIN)
dwtest(be_modelLIN)

#On s'apercoit que la série est non stationnaire, on va donc tenter de faire une régression de premiere différence. 
library(tseries)
adf.test(pdata$EARNIMP4, k=2)
first_diff_model <- plm(formula, 
                        data = pdata, model = "fd")
summary(first_diff_model)

#GMM :
install.packages("plm")
install.packages("pgmm")

library(plm)
library(pgmm)

# Estimation GMM dynamique
gmm_model <- pgmm(EARNIMP4 ~ lag(EARNIMP4, 1) + ORANGYR + FRUITMO + BREAKFAST + 
                    HRSLEEP + VIG10DMIN + EDUC + SEX + AGE + I(AGE^2) |
                    lag(EARNIMP4, 2:5),  # Instruments: retards 2 à 5
                  data = pdata, effect = "individual", model = "twosteps")

# Résumé du modèle GMM
summary(gmm_model)

#Multinomial
install.packages("mlogit")   # Pour le modèle logit multinomial
install.packages("nnet")     # Régression multinomiale

library(mlogit)
library(nnet)
# Conversion des données au format mlogit
data_mlogit <- mlogit.data(pdata, choice = "EARNIMP4", shape = "long", alt.levels = unique(data$EARNIMP4))

# Estimation d'un modèle Multinomial Logit avec effets fixes
mlogit_model <- mlogit(EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST + HRSLEEP + 
                         VIG10DMIN + EDUC + SEX + AGE + I(AGE^2) + EARNIMP4_lag | 0, 
                       data = data_mlogit, reflevel = "1")

summary(mlogit_model)





#Test d'autocorréaltion
install.packages("geepack")
library(lme4)
library(lme4)
l1 <- lmer(formula, data = pdata) 
lmer.display(l1, ci.ranef = T)



###Test de Pesaran

pcdtest(be_modelLIN, test = c("cd"))



vif(fe_modelLIN)
# Estimateur Within total
b_within <- plm(formula , data = pdata, model="within", effect="twoways")
summary(b_within)
coeftest(b_within)

##Within individuel 
b_within_i <- plm(formula, data = pGrunf4, model="within", effect="indiv")
summary(b_within_i)

####A noter : la dummysation pour le sexe. 
cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between", effect = "time")
#Model=type de modèle en BETWEEN EFFECT
#Ici R2 de 0,90.  
#Mais significativité. 
cat("Résumé du modèle Between Effects individuel :\n")
print(summary(be_modelLIN))

###EFFETS NON LINEAIRES : 

# Définition de la formule (même que précédemment)
formula <- EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST + SNACKS + HRSLEEP +
  VIG10DMIN + EDUC + SEX + AGE +I(AGE^2)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLIN <- plm(formula, data = pdata, model = "between", effect = "time")
#Model=type de modèle en BETWEEN EFFECT
#Ici R2 de 0,90.  
#Mais significativité. 
cat("Résumé du modèle Between Effects individuel :\n")
print(summary(be_modelLIN))

formula <- EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST + SNACKS + HRSLEEP+I(HRSLEEP^2) +
  VIG10DMIN + EDUC + SEX + AGE +I(AGE^2)
cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLINPOLY <- plm(formula, data = pdata, model = "between", effect = "time")
#Model=type de modèle en BETWEEN EFFECT
#Ici R2 de 0,90.  
#Mais significativité. 
cat("Résumé du modèle Between Effects individuel :\n")
print(summary(be_modelLINPOLY))

####Exhaustive search sur toute la base de données 

# Installer leaps si nécessaire
library(leaps)
# Recherche exhaustive sur toutes les variables explicatives
model <- regsubsets(EARNIMP4 ~ ., data = data, nbest = 4, method = "exhaustive")

# Résumé des meilleurs modèles
summary_model <- summary(model)

# Affichage des meilleurs modèles
print(summary_model)

cat("\nAjustement du modèle Between Effects (BE) sur les individus avec log(EARNIMP4)...\n")
be_modelLINPOLY <- plm(formula, data = pdata, model = "between", effect = "indiv")
#Model=type de modèle en BETWEEN EFFECT
#Ici R2 de 0,90.  

####Modeles avec variable dependante Y limitée: 

#1:FGLS à tester pour corriger l'hétéroscédasticité. 
# Charger le package plm (si ce n'est pas déjà fait)
if(!require(plm)) install.packages("plm")
library(plm)

# Définition de la formule (même que précédemment)
formula <- EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST  + HRSLEEP +
  VIG10DMIN + EDUC + SEX + AGE







###FGLS: 

fe_twoLOG <- plm(formula, data = pdata, model = "within", effect = "twoways")
summary(fe_twoLOG)

# Estimation du modèle FGLS entre (using pggls)
fgls_model <- pggls(formula, data = pdata, model = "between",effect="time")

# Afficher le résumé du modèle FGLS
summary(fgls_model)

library(lme4)

# Modèle Between avec un effet aléatoire
model_lme_between <- lmer( formula + 1 | SERIAL),
                          data = pdata)

# Résumé du modèle
summary(model_lme_between)
,               


library(plm)
library(car)
library(lmtest)
library(sandwich)
library(nlme)

best_model_selector <- function(data, formula, index) {
  # Liste des modèles testés
  models <- list()
  vif_values <- list()
  r2_values <- list()
  valid_models <- list()
  
  # 1. Modèle OLS
  model_ols <- lm(formula, data = data)
  models$OLS <- model_ols
  vif_values$OLS <- max(vif(model_ols), na.rm = TRUE)
  r2_values$OLS <- summary(model_ols)$adj.r.squared
  
  # 2. Modèle Between
  model_between <- plm(formula, data = data, index = index, model = "between")
  models$Between <- model_between
  vif_values$Between <- max(vif(lm(formula, data = data)), na.rm = TRUE)  # VIF estimé sur OLS
  r2_values$Between <- summary(model_between)$r.squared
  
  # 3. Modèle Within (Effets Fixes - Individuel)
  model_within_indiv <- plm(formula, data = data, index = index, model = "within", effect = "individual")
  models$Within_Indiv <- model_within_indiv
  vif_values$Within_Indiv <- max(vif(lm(formula, data = data)), na.rm = TRUE)
  r2_values$Within_Indiv <- summary(model_within_indiv)$r.squared
  
  # 4. Modèle Within (Effets Fixes - Temporel)
  model_within_time <- plm(formula, data = data, index = index, model = "within", effect = "time")
  models$Within_Time <- model_within_time
  vif_values$Within_Time <- max(vif(lm(formula, data = data)), na.rm = TRUE)
  r2_values$Within_Time <- summary(model_within_time)$r.squared
  
  # 5. Modèle Within (Effets Fixes - Deux Voies)
  model_within_twoways <- plm(formula, data = data, index = index, model = "within", effect = "twoways")
  models$Within_Twoways <- model_within_twoways
  vif_values$Within_Twoways <- max(vif(lm(formula, data = data)), na.rm = TRUE)
  r2_values$Within_Twoways <- summary(model_within_twoways)$r.squared
  
  # 6. Modèle GLS
  model_gls <- gls(formula, data = data, correlation = corCompSymm(form = ~ 1 | data[[index[1]]]), method = "REML")
  models$GLS <- model_gls
  vif_values$GLS <- max(vif(lm(formula, data = data)), na.rm = TRUE)
  r2_values$GLS <- summary(model_gls)$r.squared
  
  # Vérification des modèles valides
  for (m in names(models)) {
    model <- models[[m]]
    if (r2_values[[m]] > 0.5 && vif_values[[m]] < 10) {
      coefs <- summary(model)$coefficients
      p_vals <- coefs[, 4]  # p-values
      if (sum(p_vals > 0.05, na.rm = TRUE) <= 3) {
        valid_models[[m]] <- model
      }
    }
  }
  
  # 7. Sélection du modèle final
  if (length(valid_models) > 0) {
    best_model <- valid_models[[which.max(sapply(valid_models, function(m) summary(m)$adj.r.squared))]]
  } else {
    # Choisir le modèle avec le R² ajusté le plus élevé et VIF < 10
    best_model_name <- names(which.max(r2_values))
    best_model <- models[[best_model_name]]
  }
  
  return(best_model)
}



best_model <- best_model_selector(data, formula, index)
summary(best_model)


#2:modele logitordonné 
library(plm)
install.packages("ordinal")
library(ordinal)
install.packages("oglmx")
library(oglmx)
install.packages("lmtest")
library(lmtest)


formula <- as.formula(paste("factor(", all.vars(formula)[1], ") ~", paste(all.vars(formula)[-1], collapse=" + ")))

model_ordinal_fe <- clm(EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST  + HRSLEEP +
                          VIG10DMIN + EDUC + SEX + AGE, data=pdata)
summary(model_ordinal_fe)


###LOGIT ORDERED 
install.packages(c("mlogit", "dplyr", "data.table", "Rcrologit"))

library(mlogit)      # Modélisation des choix discrets
library(dplyr)       # Manipulation des données
library(data.table)  # Gestion efficace des grands datasets
library(Rcrologit)   # Modélisation du conditional rank-ordered logit

# Transformation et préparation des données pour le modèle logit conditionnel
dataprep <- dataPrep(
  data,
  idVar = "SERIAL",  # Identifiant unique de chaque individu
  rankVar = "rank",  # Variable représentant l’ordre des préférences
  altVar = "alternative",  # Variables alternatives considérées dans le modèle
  covsInt.fix = list("Gender"),  # Covariables fixes interagissant avec les alternatives
  covs.fix = list("log_Wage"),  # Variables explicatives fixes
  FE = c("Firm_ID")  # Effets fixes au niveau de l’entreprise
)
# Vérifier et transformer EARNIMP4 en facteur ordonné
data$EARNIMP4 <- as.ordered(data$EARNIMP4)

# Vérifier et transformer persons en facteur si nécessaire
data$SERIAL <- as.factor(datapersons)

# Modèle de régression logistique ordonnée
polrModel <- polr(formula, 
                  data = training, Hess = TRUE)

# Résumé des résultats
summary(polrModel)



summary(data)

###ORDONNE DE PANEL : 
install.packages("VGAM")
library(VGAM)
# Modèle Probit Ordonné
model <- vglm(formula, family=propodds(probit), data=pdata)

# Résumé du modèle
summary(model)

###On le justifie parce que la variable Y est ordonnée de 1 à 20 ici ! 

###3 Tobit de panel
library(pglm)

data('pdata', package = 'pglm')
model_ordonné <- pglm(formula,  # Variables explicatives
                      data = pdata, 
                      family = ordinal("probit"),
                      R=5,# Modèle Tobit (censuré),
                      print.level  =  3 ,
                      method = "bfgs",
                      model = "within" )  # Algorithme d'optimisation
summary(model_ordonné)
fixed <- c(TRUE, FALSE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) 


n <- length(coef(formula))  # Nombre de variables explicatives dans le modèle
start_values <- rep(0, n)   # Crée un vecteur de zéros avec n éléments (un pour chaque paramètre)
model_ordonné <- pglm(formula, 
                      data = pdata, 
                      family = ordinal("probit"),
                      R = 5,
                      print.level = 3,
                      method = "bfgs",
                      model = "within",
                      start = start_values,
                      fixed)

# Créer des valeurs initiales (par exemple, des zéros)
start_values <- rep(0, length(coef(formula)))  # Ou utilisez une régression ordinaire pour une estimation plus précise

library(pglm)
library(plm)  # Pour gérer les données en panel

# Vérifier la structure des données
str(pdata)
head(pdata)

pdata <- pdata.frame(pdata, index = "serial")
model_ordonné <- pglm(formula, 
                      data = pdata, 
                      family = ordinal("probit"),
                      R = 5,
                      print.level = 3,
                      method = "bfgs",
                      model = "within",
                      start = rep(0, length(coef(lm(formula, data = pdata)))))

model_ordonné <- pglm(formula, 
                      data = pdata, 
                      family = ordinal("probit"),
                      R = 5,
                      print.level = 3,
                      method = "nr",
                      model = "between")


# Rank-ordered logit
dataprep <- dataPrep(data, idVar = "SERIAL", rankVar = "rank",
                     altVar = "alternative",
                     covsInt.fix = list("AGE,SEX"),
                     covs.fix = list("log_Wage"), FE = c("Firm_ID"))

rologitEst <- rcrologit(dataprep)





# Charger le package brms (et ses dépendances)
install.packages("brms")
library(brms)

# Vérifiez la structure de vos données (optionnel)
str(pdata)

# Estimation du modèle Tobit en panel avec brms
# Ici, la syntaxe "y | cens(cens)" permet de préciser la variable indiquant la censure.
fit_tobit <- brm(formula = bf(EARNIMP4~ ORANGYR + FRUITMO + BREAKFAST + SNACKS + HRSLEEP + VIG10DMIN + EDUC + SEX + AGE,
                              data = pdata,
                              family = gaussian(),   # le modèle Tobit classique repose sur une loi normale pour le latent
                              chains = 2,            # ajustez le nombre de chaînes selon vos besoins
                              iter = 2000,           # nombre d'itérations par chaîne
                              seed = 123             # pour la reproductibilité
)

# Affichage du résumé du modèle
summary(fit_tobit)

# Charger le package brms
library(brms)

# Assurez-vous que la variable de réponse est un facteur
pdata$EARNIMP4 <- as.factor(pdata$earnimp4)

# Exemple avec une variable explicative "x" (remplacez "x" par vos prédicteurs si besoin)
# Estimation du modèle multinomial
fit_multinom <- brm(
  formula = EARNIMP4 ~ ORANGYR + FRUITMO + BREAKFAST + SNACKS + HRSLEEP +
    VIG10DMIN + EDUC + SEX + AGE,
  data = pdata,
  family = categorical(),  # Indique un modèle multinomial
  chains = 2,              # Pour un test rapide ; augmentez (ex. à 4) pour l'analyse finale
  iter = 2000,             # Nombre d'itérations par chaîne
  seed = 123
)

# Affichage du résumé du modèle
summary(fit_multinom)




#####Nouvelles idées améliorer le remplacement de départ par la médiane. 

library(VIM)  
newdata <- data[EARNIMP4,ORANGYR,FRUITMO,BREAKFAST,SNACKS,HRSLEEP,VIG10DMIN,EDUC,SEX,AGE]
dataimputed <- kNN(newdata, k = 3)  # Ajuster k pour la vitesse







###TESTS DE PERFORMANCE : 

# Existence  :F-test entre pooled et within 
pFtest(fe_indiv_LOG,pooled_model)
#Il apparaît évident que l'effet fixe est préféré 
# Comparaison des modeles avec R2 et Fischer les plues eleves  : 
pFtest(be_modelLIN,fe_indiv_LOG)

#Test notamment lié à l'effet temporel ? 
plmtest(be_modelLIN, effect = "time", type = "bp")

# F-test pour les effets temporels
pFtest(fe_timeLOG, pooled_model)





install.packages("effectsize")
library(effectsize)

# Convertir automatiquement en coefficients non standardisés
unstandardized_coeffs <- unstandardize_parameters(be_modelLIN,data=pdata)

print(unstandardized_coeffs)

# Test de Hausman entre effet fixe et aléatoire. 
cat("\nTest de Hausman entre le modèle à effets fixes (FE) et à effets between")
hausman_test <- phtest(fe_modelLIN, re_modelLIN)

#Aleatoire vsOLS : Afficher les résultats du test de Hausman
print(hausman_test)

###Le test de Hausman, avec une p-value inférieur à 0,05.
###On rejette les effets aléatoires, et on préfère le modèle à effet fixe within
###Ainsi, même si le R2 d'aléatoire est plus élevé, Hausman souligne un impact causal plus élevé du Within
#En soit le R2, ou R2 ajusté qui sont proches ici ; ne sont pas forcément la mesure la plus importante . 


###Tests pour l'aléatoire
library(plm)
library(lmtest)

cat("\nTest de Breusch-Pagan pour choisir entre Pooled OLS et Random Effects...\n")
bp_test <- plmtest(pooled_modelLIN, type = "bp")
print(bp_test)

#La p-value est faible, le random effect est préférable au modèle poolé 

####Test de Mundlack 
# Test de Breusch-Pagan pour les effets individuels
plmtest(pooling, type = "bp")

# Comparaison des modèles avec stargazer (facultatif)
if ("stargazer" %in% installed.packages()) {
  library(stargazer)
  cat("\nComparaison des modèles :\n")
  stargazer(pooled_modelLIN, fe_modelLIN, re_modelLIN, be_modelLIN, 
            type = "text", title = "Comparaison des modèles d'estimation panel")
}

####Test d'autocorrélation pour le Within. 
install.pacakges("lmtest")
library(lmtest)
bgtest(pooled_modelLIN)  # Test de Breusch-Godfrey sur le modèle Pooled OLS
###On peut conclure qu'il y a autocorrélation pour le Within. 
bgtest(fe_modelLIN) 
#Il y a un effet fixe dans Lmtest !

#Test de Mundlak 
cat("\nTest de Mundlak pour vérifier la corrélation entre effets individuels et variables explicatives...\n")
mundlak_test <- plm(formula, data = pdata, model = "random")
mundlak_test <- update(mundlak_test, . ~ . + I(fitted(re_modelLIN)))
print(summary(mundlak_test))

###Reprendre les exercices : qui sera publié !*




###Interpréter le modele au R2 de 0,90 ajusté. 
# Charger les packages nécessaires
library(plm)
library(lmtest)
library(sandwich)
test_results <- coeftest(be_modelLIN, vcov = vcovBK(be_modelLIN, type = "HC3"))

# Affichage des résultats du test
print(test_results)

library(data.table)
#Calculer les écarts types. 
ecarts_types <- data[, lapply(.SD, sd, na.rm = TRUE)]
print(ecarts_types)




#Interprétations des coefficients du meilelure modele : à savoir le between idnividuel

# Charger les packages nécessaires
library(plm)
library(lmtest)

summary(be_modelLIN)
# Test des coefficients avec des erreurs standards robustes
# Ici, nous utilisons la fonction vcovHC pour obtenir des erreurs robustes
test_results <- coeftest(be_modelLIN, type="between", vcov = vcovHC(be_modelLIN, method = "arellano", type = "HC0"))

# Affichage des résultats du test
print(test_results)

#Méthode 2 : HAC : 
vcov_hac <- vcovNW(be_modelLIN, lag = 2, prewhite = TRUE)
new<-coeftest(be_modelLIN, vcov. = Newestwey)
print(new)


vcov_robuste <- vcovHC(modele, type = "HC1")

vcov_robuste <- vcovHC(be_modelLIN, type = "HC1")



#Test d'autocorrélation
library(plm)
library(lmtest)

pdwtest(be_modelLIN)
























####Test de stationnarité
install.packages("tseries")
library(tseries)

cat("\nTest de stationnarité Levin-Lin-Chu sur la variable dépendante...\n")
llc_test <- levinlin.test(pdata$EARNIMP4)
print(llc_test)




####


####Partie corrélation : 

# Chemin vers le fichier de données
file_path <- "C:/Users/grego/OneDrive/Documents/sampled_data1.csv"
correlation_output_path <- "C:/Users/grego/OneDrive/Documents/correlation_matrix.txt"

# Charger les bibliothèques nécessaires
library(readr)

# Fonction pour calculer et exporter la matrice de corrélation
compute_and_export_correlation_matrix <- function(file_path, output_path) {
  # Charger les données
  cat("Chargement des données...\n")
  data <- read_csv(file_path)
  
  # Sélectionner uniquement les colonnes numériques
  cat("Sélection des colonnes numériques...\n")
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # Vérifier qu'il y a des colonnes numériques
  if (ncol(numeric_data) == 0) {
    stop("Aucune colonne numérique trouvée dans la base de données.")
  }
  
  # Calculer la matrice de corrélation
  cat("Calcul de la matrice de corrélation...\n")
  correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  # Exporter la matrice dans un fichier texte
  cat("Exportation de la matrice de corrélation...\n")
  write.table(round(correlation_matrix, 2), file = output_path, sep = "\t", quote = FALSE)
  cat(sprintf("Matrice de corrélation exportée avec succès dans '%s'.\n", output_path))
  
  # Retourner la matrice de corrélation pour affichage éventuel
  return(round(correlation_matrix, 2))
}

# Calculer et exporter la matrice de corrélation
correlation_matrix <- compute_and_export_correlation_matrix(file_path, correlation_output_path)

# Afficher la matrice dans la console
cat("Matrice de corrélation :\n")
print(correlation_matrix)

###Corrélations entre le revenu et le reste des variables
# Installer et charger les packages nécessaires (si ce n'est déjà fait)
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Définir les chemins d'accès aux fichiers
# Remplacez par le chemin de votre fichier de données
correlation_output_path <- "C:/Users/grego/OneDrive/Documents/matrice_corr.csv" # Remplacez par le chemin souhaité pour l'export


# Calculer la matrice de corrélation en utilisant uniquement les observations complètes
correlation_matrix <- cor(data, use = "complete.obs")

# Exporter la matrice de corrélation dans un fichier CSV
write.csv(correlation_matrix, correlation_output_path, row.names = TRUE)

# Afficher la matrice complète dans la console
cat("Matrice de corrélation :\n")
print(correlation_matrix)

# Extraction et affichage des corrélations entre 'revenu' et les autres variables
if ("revenu" %in% colnames(data)) {
  revenu_correlations <- correlation_matrix[, "revenu"]
  cat("\nCorrélation entre 'revenu' et le reste des variables :\n")
  print(revenu_correlations)
} else {
  cat("\nLa colonne 'revenu' n'existe pas dans les données.\n")
}



###Corrélation liées uniquement aux variables du système afin de vérifier multicolinéarité parfaite. 
data <- read_csv("C:/Users/grego/OneDrive/Documents/sampled_data1.csv") %>% select(all.vars(EARNINGS ~ VEGMO + ORANGYR + FRUITMO + BREAKFAST + SNACKS + HRSLEEP + VIG10DMIN + EDUC))
print(round(cor(data, use = "pairwise.complete.obs"), 2))
#On peut observer au dela de probleme de valeurs manquantes, une relation entre le snacks et breakfast trop forte
#Cela paraît logique, étant donné que des personnes aiment bien le salé au petit-déjeuner, notamment aux USA

#Le nombre d'heures de sommeil est corrélé positivement à breakfast. 
#OrangeYR apparaît être trop peu reliée pour être considérée comme une variable omise. 
#On peut remarque forte corrélation entre la variable FRUIT ET LEGUMES 
#Cela pousse fortement à faire une ACP afin de résumer au mieux

####GMM Arellano-bond
install.packages("plm")
install.packages("AER")   # Pour tester l'autocorrélation
install.packages("pgmm")  # Implémentation du GMM
library(plm)
library(AER)
library(pgmm)

data("EmplUK", package = "plm")


# Spécification du modèle Arellano-Bond (GMM) avec pgmm
gmm_model <- pgmm(formula |lag(log(BREAKFAST), 2:7),
  data = data,
  effect = "twoways",
  model = "twosteps"
)

# Résumé du modèle
summary(gmm_model)

