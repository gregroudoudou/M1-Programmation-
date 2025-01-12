# M1-Programmation-

##
Ce dossier contient l'ensemble des documents permettant afin de lancer et comprendre une application de statistiques automatisées ;dans le cadre notamment de tests de différences de moyennes  entre deux populations. Cela permet notamment de savoir si on a un échantillon représentatif de la population générale par exemple. 
Ainsi, la partie de tests statistiques permet essentiellement de vérifier la qualité des données, avant de pouvoir les utiliser. 

De plus, une seconde partie a été ajoutée, dans la continuité de la 1ere , qui permet notamment de comparer la performances des modèles. 

Au final, le but essentiel est  de pouvoir améliorer les choix en matière de données d'un côté, en tentant de comparer des échantillons avec les moyennes nationales par exemple et  par la suite, d'améliorer également la phase de modélisation . En effet, on peut ainsi charger n'importe quelle base de données et pouvoir effectuer rapidement et facilement plusieurs régressions. 



# Cadre du projet
Ce travail a été effectué dans le cadre d'un projet en Master DS2E à Strasbourg durant le 1er semestre de 2024 par Grégoire Fuchs . 
Il sera présenté le 14 Janvier 2025. 


# Prérequis 
Afin de pouvoir charger  les packages, il est important d'installer avant l'ensemble des packages nécessaires. 

Ainsi , il vous suffit de copier ces lignes de code afin d'installer les packages nécessaires.

```r 
 #Liste des packages requis
packages_requis <- c("rstatix", "DescTools", "stats", "shiny", "shinyjs", "DT", "rintrojs")

### Fonction pour installer et charger les packages
installer_et_charger <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
      message(paste("Le package", pkg, "a été installé et chargé."))
    } else {
      message(paste("Le package", pkg, "est déjà installé et chargé."))
    }
  }
}

### Exécution de la fonction
installer_et_charger(packages_requis)

### Installation du code R et lancement des applications. 

```

###Construit avec 
-Rshiny
-Rmarkdown
-Flexdashboard
-de nombreux packages de tests statistiques : Rstatix, 


##Auteur
Grégoire Fuchs 

## Licence

Ce projet est sous licence MIT - voir le fichier LICENSE pour plus de détails

# Remerciements

* Un grand merci à tous les utilisateurs ! 

: 
-le code 
-les données d'exemples 
-les résultats sous forme d'un diaporama effectué avec flexdashboard 
