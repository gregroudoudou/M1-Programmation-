# M1-Programmation-

#Description courte du projet 
Ce dossier a pour but de permettre de lancer une application qui va automatiser l'exécution de tests statististiques , permettant de sélectionner des données de qualité , autant que des liaisons entre les variables significatives. De plus, il est également proposé de pouvoir comparer par la suite des modèles de régression, à partir des données utilisées. 
# Structure des dossiers et des fichiers du Github 

Ainsi, le code R permet de lancer directement chacune des applications Shiny. 
Ensuite, le code Rmarkdown , permet de créer un dashboard interactif, qui permet donc d'intégrer autant les applications RShiny que l'explication du code en lui-même, dans ses grandes lignes. 
Enfin, ce readme permet de rapidement lancer l'application . 

## Lancement rapide  

### Vérifier ses pré-Requis en termes de packages 
Le 1er pré-requis est d'avoir la version de Rstudio la plus récente, ainsi que les packages nécessaires, rappelés ci-dessous. 
Le document R ou Rmarkdown contient normalement cette étape, voici un code similaire si vous avez loupez cette étape. De nombreux packages sont nécessaires afin de pouvoir effectuer les tests statistiques. 
L'application a été réalisée sous l'IDE Rstudio, qui a également créé le langage Rshiny, son téléchargement est donc recommandé. 
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


### Exemples de résultats après avoir chargé l'application 
Après avoir téléchargé sur Github le fichier R, et si vous souhaitez utiliser une autre base de données, double-cliquez sur un des fichiers .R qui permettra de lancer l'application et de le charger sous votre IDE préféré. 

Si Rshiny a déjà été installé et chargé , ce qui signifie que vous avez donc suivi l'étape suivante d'installation des packages, il devra s'afficher normalement un bouton supplémentaire, à côté du run, dénommé Run App, comme indiqué ici sous Rstudio. 

### Lancer la première application R Shiny . 
Après avoir téléchargé  le fichier ",et d'avoir installé autant que charger les différents packages sous R ,  il vous suffit de cliquer sur run dans le fichier R afin de lancer l'application Shiny qui s'appelle Logiciel de statistiques. 

### Lancer la deuxième application R Shiny. 
- Après avoir téléchargé  le fichier " , et d'avoir installé autant que charger les différents packages sous R
- il vous suffit de cliquer sur run dans le fichier R afin de lancer l'application Shiny qui s'appelle Logiciel de statistiques. 

# Démarre rapidement le dashboard avec le fichier Rmarkdown

Etant donné qu'il y avait un nombre important d'erreurs sous Rmarkdown, afin de pouvoir lancer l'exécution de tests statistiques. 



# Exemples d'applications. 

## 1er posssibilité d'utilisation  : comparer une moyenne d'échantillon avec une moyenne de population : la valeur de référence. 

La première application de statistiques automatisées ;dans le cadre notamment de tests de différences de moyennes  entre deux populations. Cela permet notamment de savoir si on a un échantillon représentatif de la population générale par exemple. 

### 1ere  étape:  Sélection de l'utilisateur 

Afin de pouvoir effectuer le test, l'utilisateur doit choisir en premier la moyenne d'échantillon (Y) et en deuxième, la moyenne de population avec la possibilité de mettre une valeur de référence (X). 
Par exemple H0  : mu0=Mu1 si l'échantillon est représentatif. 
H1 : où valeur de référence rentrée par l'individu mu0. De plus, la valeur mu1, est notamment celle de la moyenne de la variable.

### 2eme étape : définir le type de variable sélectionné et trouver automatiquement le test statistique. 
Après avoir lancé l'application, l'application Shiny permet notamment de sélectionner le test statistique qui sera le plus approprié. 
Ainsi, le 1ere critère sera par exemple le respect de l'hypothèse de normalité de la variable Y . Un texte s'affiche automatiquement afin d'indiquer à l'utilisateur si la variable Y suit une loi normale ou pas. Ainsi, cela permet de savoir si on va utiliser un test paramétrique (distribution normale) ou non paramétrique. 

Ensuite, entre en jeu le type de variable Y : si la variable est binaire, quantiative ou multinomiale.Certains tests , en effet, notamment paramétriques, reposent d'emblée sur les régressions sous-jacentes. Ensuite, la variable X , explicative est choisie :elle peut être dichotomique, quantitative ou catégorielle.Cela permet ainsi d'effectuer le test statistique adéquat, et de mettre en place des étapes, parfois oubliées, comme le respect des hypothèses. 

### 3.Résultat final
Le résultat final du test est alors affiché dans le cadre. Afin de pouvoir afficher l'ensemble des résultats de chaque test (intervalles de confiances, degrés de libertés...), qui sont souvent propres à chaque test statistique, j'ai décidé de garder l'encadré originel. 


## 2eme possibilité d'utilisation : tester la significativité entre les variables . 
Il suffit cette fois-ci pour l'utilisatuer de sélectionner la variable expliquée et explicative , ce qui permet donc de tester la significativité. 

#2eme application : 
#Charger le fichier R
#Mettre en place les données. 
Les données peuvent être chargées sous différents formats : R, csv....
#
: la comparaison des performances (R2, Fischer...) entre différentes régresssions multiples, logistiques , avec sélection individuelle des variables. 

Dans le but d'une régression, 
 
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
