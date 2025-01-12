# M1-Programmation-

#Description courte du projet 
Ce dossier a pour but de permettre de lancer une application qui va automatiser l'exécution de tests statististiques , permettant de sélectionner des données de qualité , autant que des liaisons entre les variables significatives. De plus, il est également proposé de pouvoir comparer par la suite des modèles de régression, à partir des données utilisées. 

##Pré-Requis en termes de packages 

Ainsi , il vous suffit de copier ces lignes de code afin d'installer les packages nécessaires et de pouvoir lancer l'application avec l'onglet run .

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
###Structure des dossiers et des fichiers

Ainsi, le code R permet de lancer directement chacune des applications Shiny. 
Ensuite, le code Rmarkdown , permet de créer un dashboard interactif, qui permet donc d'intégrer autant les applications RShiny que l'explication du code en lui-même, dans ses grandes lignes. 

#Exemples d'applications. 
## 1er posssibilité d'utilisation  : comparer une moyenne d'échantillon avec une moyenne de population : la valeur de référence. 

La première application de statistiques automatisées ;dans le cadre notamment de tests de différences de moyennes  entre deux populations. Cela permet notamment de savoir si on a un échantillon représentatif de la population générale par exemple. 

### 1ere  étape:  Sélection de l'utilisateur 

Afin de pouvoir effectuer le test, l'utilisateur doit choisir en premier la moyenne d'échantillon (Y) et en deuxième, la moyenne de population avec la possibilité de mettre une valeur de référence (X). 
Par exemple H0  : mu0=Mu1 si l'échantillon est représentatif. 
H1 : où valeur de référence rentrée par l'individu mu0. De plus, la valeur mu1, est notamment celle de la moyenne de la variable.

### 2eme étape
Après avoir lancé l'application, l'application Shiny permet notamment de sélectionner le test statistique qui sera le plus approprié. 
Ainsi, le 1ere critère sera par exemple le respect de l'hypothèse de normalité de la variable Y . Un texte s'affiche automatiquement afin d'indiquer à l'utilisateur si la variable Y suit une loi normale ou pas. Ainsi, cela permet de savoir si on va utiliser un test paramétrique (distribution normale) ou non paramétrique. 

Ensuite, entre en jeu le type de variable Y : si la variable est binaire, quantiative ou multinomiale.Certains tests , en effet, notamment paramétriques, reposent d'emblée sur les régressions sous-jacentes. Ensuite, la variable X , explicative est choisie :elle peut être dichotomique, quantitative ou 

## 2eme possibilité d'utilisation : tester la significativité entre les variables . 



 
Ainsi, la partie de tests statistiques permet essentiellement de vérifier la qualité des données, avant de pouvoir les utiliser. 
De plus, une seconde partie a été ajoutée, dans la continuité de la 1ere , qui permet notamment de comparer la performances des modèles. 

Au final, le but essentiel est  de pouvoir améliorer les choix en matière de données d'un côté, en tentant de comparer des échantillons avec les moyennes nationales par exemple et  par la suite, d'améliorer également la phase de modélisation . En effet, on peut ainsi charger n'importe quelle base de données et pouvoir effectuer rapidement et facilement plusieurs régressions. 



# Cadre du projet
Ce travail a été effectué dans le cadre d'un projet en Master DS2E à Strasbourg durant le 1er semestre de 2024 par Grégoire Fuchs . 
Il sera présenté le 14 Janvier 2025. 


# Prérequis 
#1er prérequis : avoir une  base de données
La base de données mtcars, qui est une base de données qui décrit les caractéristiques de nombreuses voitures est d'emblée pré-chargée. 
Lorsqu'un utilisateur souhaite charger une autre base de données, il doit l'importer sous formes de bases de données au format csv , .txt, .xslx, .rds, .sas7dat,.sav. Ainsi, la majorité des formats de base de données sont acceptés. 




Afin de pouvoir charger  les packages, il est important d'installer avant l'ensemble des packages nécessaires. 


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
