#Packages nécessaires pour les tests statistiques
library(shiny)
library(rstatix)
library(DescTools)
library(stats)
library(shinyWidgets)


##But de l'application : calculer des statistiques paramétriques et semi-paramétriques
#En ayant une sélection automatique du test (àl'exception de l'appariemment ou non )
#Il contient pas le T de Student à 2 échantillons ni Fisher étant donné que c'est souvent effectué automatiquement dans Summary. 
#Penser à installer ShinyJs avant pour certaines options.
#Faire install.packages(shinyJs) si besoin
library(shinyjs)

#Lancer l'application d'emblée : 
run_Statistiques <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}

diagnose <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}


app_ui <- function() {
  shiny::fluidPage(
    rintrojs::introjsUI(),
    # Je crée le titre de l'application
    shiny::titlePanel(shiny::tags$h3("Statistiques : l'app de  stats", windowTitle = "Statistiques : l'app de stats")),
    shiny::titlePanel(shiny::tags$h5(

    )
    ),
    # Je charge la librairie javscript de Shiny
    shinyjs::useShinyjs(),
    
    # Met en place la barre latérale qui permet de proposer de charger le Fichier
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # shiny::fileInput("file", "Upload a file"),
        
        shiny::actionButton("launch_modal", "Importation des données"),
        
        # Je mets en place le menu déroulant pour choisir la varaible expliquéeY
        rintrojs::introBox(
          shiny::uiOutput("expliquéeY_var_dropdown"),
          data.step = 3,
          data.intro = "Choose the expliquéeY variable from this dropdown."
        ),
        
        # Mettre le texte en-dessous 
        rintrojs::introBox(
          shiny::textOutput("expliquéeY_var_text"),
          data.step = 4,
          data.intro = "Cela est la variable expliquéeY"
        ),
        
        # Menu déroulant pour choisir la variable explicativeX
        rintrojs::introBox(
          shiny::uiOutput("explicativeX_var_dropdown"),
          data.step = 5,
          data.intro = "Choisit la variable explicativeX X "
        ),
        rintrojs::introBox(
          shiny::textOutput("explicativeX_var_text"), 
          data.step = 6,
          data.intro = "Cela permet de mettre en place la variable X  "
        ),
        
        # Champ de la saisie de la moyenne hypothètique qui est utile dans certain cas (ne s'actionne que si test du Wilcoxon)  
        rintrojs::introBox(
          shiny::uiOutput("input_mean"),
          data.step = 7,
          data.intro = "Entre la moyenne ici."
        ),
        shiny::conditionalPanel(
          condition = "input.statistical_test == 'Repeated measures ANOVA (paired)' || input.statistical_test == 'Multilevel Logistic Regression (paired)' || input.statistical_test == 'Friedman\\'s ANOVA II (paired)'",
          rintrojs::introBox(
            shiny::uiOutput("identifier_dropdown"),
            data.step = 8,
            data.intro = "Choisissez l'identifiant dans ce menu déroulant."
          )
        ),
        
        # Nouveau menu déroulant pour choisir le test statistique
        rintrojs::introBox(
          shiny::uiOutput("statistical_test_dropdown"),
          data.step = 9,
          data.intro = "Choisir le test statistique dans ce menu déroulant"
        ),
        
        # Endroit pour afficher le résultat du test 
        rintrojs::introBox(
          shiny::verbatimTextOutput("test_report"),
          data.step = 10,
          data.intro = "C'est l'endroit pour afficher les résultats du test. "
        )
      ),
      
      # Afficher les données importées de manière interactive (maxium 12 colonnes)
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(width = 12, DT::dataTableOutput("dataTable")),
          shiny::column(width = 12, shiny::plotOutput("expliquéeY_var_histogramme"))
        ),#Permet d'afficher également  le graphique d'histogrammeme, qui est chargé dans la partie Server. 
      )
    )
  )
}

#Partie 1 : tentons de déterminer automatiquement,après sélections de variables, le test statistique 
#le test statistique le plus adapté (normalité). 

# Afficher un tableau de données

display_data_table <- function(data) {
  DT::datatable(data)
}


# Etape 1 : déterminer le type de variable  et afficher une phrase sur l'interface (après mise à jour dans l'UI)
#le type de la variable expliquéeY afin d'appliquer le test statistique adapté. 

determine_expliquéeY_variable <- function(expliquéeY_var) {
  # Vérifions d'abord si la variable est numérique. 
  if (is.numeric(expliquéeY_var)) {
    # Vérifions si elle suit une loi Normale. 
    shapiro_test <- stats::shapiro.test(expliquéeY_var)

    # Or si pvalue<0,05 du test de Shapiro, cela  montre du coup qu'on a une normalité des résidus. 
    if (shapiro_test$p.value < 0.05) {
      "L'hypothèse de normalité est violée"
    } else {
      "L'hypothèse de normalité est vérifiée"
    }
  } else if (is.character(expliquéeY_var)) {
    # Classer si la variable Y est binaire, ou bien ordinal/nominal  
    unique_values <- length(unique(expliquéeY_var))
    if (unique_values == 2) {
      "La variable expliquéeY sélectionnée est binaire"
    } else if (unique_values > 2) {
      "La variable sélectionnée Y est multinomiale"
    } else {
      "La variable sélectionnée Y a des données invalides"
    }
  } else {
    "La variable sélectionnée n'est ni numérique ni caractère. "
  }
}

# 2 : Déterminer et afficher le type de la variable explicativeX Y

determine_explicativeX_variable <- function(explicativeX_var) {
  # Vérifie et affiche  si la variable explicativeX X sélectionnée est numérique ou pas . 
  if (is.numeric(explicativeX_var)) {
    "La variable explicativeX sélectionnée est numérique."
  } else if (is.numeric(explicativeX_var)) {
  # Calcule le nombre d'éléments distincts afin de mieux calsser la variable. 
    unique_values <- length(unique(explicativeX_var))
    if (unique_values == 2) {
      "La variable explicativeX sélectionnée est dichotomique."
    } else if (unique_values > 2) {
      "La variable explicativeX sélectionnée est catégorielle."
    } else {
      "La varaible dépendante a des données invalides"
    }
  } else {
    "La variable sélectionnée n'est ni numérique , ni une suite de caractères. "
  }
}

# Créer un graphique d'histogrammeme pour la variable dépendante
# Le paramètre est la variable indépendante.

créer_expliquéeY_variable_histogramme <- function(expliquéeY_var) {
  if (is.numeric(expliquéeY_var)) {
    graphics::hist(expliquéeY_var, main = "histogramme of expliquéeY Variable", xlab = "Values")
  }
}

#2. Mise en place de la fonction qui permet de classer les variables par types
#Elle va donc permettre de choisir le test statistique 

#Choisir un test statistique qui s'adapte correctement à notre varaible dépendante et explicativeX. 

SélectionTestStatistiques <- function(expliquéeY_var, explicativeX_var) {
  # Etape 1 : Déterminer les caractéristiques de la variable dépendante X 
  #Normalité Déterminer pour la variable numérique expliquéeY si elle suit une loi nominale ou non
  #Type : cela permet d'avoir une base pour la suite de nos tests 
  if (is.numeric(expliquéeY_var)) {
    shapiro_test <- stats::shapiro.test(expliquéeY_var)
    if (shapiro_test$p.value < 0.05) {
      expliquéeY_var_caractéristique <- "L'hypothèse de normalité est violée"
    } else {
      expliquéeY_var_caractéristique <- "L'hypothèse de normalité est vérifiée"
    }
  } else if (is.character(expliquéeY_var)) {
    unique_values <- length(unique(expliquéeY_var))
    if (unique_values == 2) {
      expliquéeY_var_caractéristique <- "binaire"
    } else if (unique_values > 2) {
      expliquéeY_var_caractéristique <- "multinomialouordinal"
    }
  } else {
    return("La variable expliquéeY n'est pas supportée")
  }


  # Etape 2 : Determiner les caractéristiques pour la variable explicativeX. 
  #On va classer suivant le nombre de groupes repérés également. 
  #Si multinomial : cela signifie notamment que l'on va devoir utiliser un test semi-paramétrique (ANOVA)
  if (is.numeric(explicativeX_var)) {
    explicativeX_var_caractéristique <- "continue"
  } else if (is.character(explicativeX_var)) {
    unique_explicativeX <- length(unique(explicativeX_var)) #Déterminer le nombre de valeur uniques. 
    if (unique_explicativeX == 1) {
      explicativeX_var_caractéristique <- "unique"
    } else if (unique_explicativeX == 2) {
      explicativeX_var_caractéristique <- "catégorielle"
    } else if (unique_explicativeX > 2) {
      explicativeX_var_caractéristique <- "catégorielle"#Permet de mettre en place potentiellement un test semi-paramétrique. 
    } else {
      #Si la variable est catégorique 
      return("La varialbe indépendante n'est pas supportée")
    }
  }


  # Utiliser ces caractéristiques pour choisir le test statistique. 
  #On place dans les deux premières boucles : la condition de normalité de Y et son types 
  #Au contraire, on place dans les secondes boucles les types de X contenu dans la variable 
  #Tout a été chargé dans les var
  if (expliquéeY_var_caractéristique == "L'hypothèse de normalité est violée") {
    if (explicativeX_var_caractéristique == "continue") {
      return("Coeff de corrélation de Spearman")
    } else if (explicativeX_var_caractéristique == "unique") {
      return("Test du signe")
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c(
        "Wilcoxon test",
        "Mann-Whitney U test 2 (non apparié)"
      ))
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c(
        # "Test d'ANOVA apparié",
        "Kruskal-Wallis test 1 (explicativeX)"
      ))
    }
  } else if (expliquéeY_var_caractéristique == "L'hypothèse de normalité est vérifiée") {
    if (explicativeX_var_caractéristique == "continue") {
      return("Coeff de corrélation de Pearson")
    } else if (explicativeX_var_caractéristique == "unique") {
      return("Tstudent 1 echantillon")
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c("explicativeX samples t-test (non apparié)", "T-test (test apparié)"))
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c("One-way ANOVA (unpaired)"
               # "analyse de la variance à mesures répétées (appariée)"
      ))
    } #Mettons en place les tests également pour les régresions logistiques. 
  } else if (expliquéeY_var_caractéristique == "binaire") {
    if (explicativeX_var_caractéristique == "unique") {
      return("Test du khi carre d'adequation")
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c("Test de McNemar (apparié)", "Chi carré test d'indépendance  avec le test de Fisher (non apparié)"))
    #Test de McNemar : permet de vérifier si les proportions entre les deux "catégories" sont significativement différentes ou non. 
    } else if (explicativeX_var_caractéristique == "catégorielle & unpaired") {
      return("Test du Chi Carré d'indépendance  et test de Fischer non apparié")
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c(
        "Cochran's Q test "
        # "Cochran's Q test (paired)"
      ))
    }
    #Le test de Cohran permet de comparer deux variables dichotomiques entre elles.
  } else if (expliquéeY_var_caractéristique == "catégorielleouordinal") {
    if (explicativeX_var_caractéristique == "unique") {
      return("Chi2 R2 test et test multinomial")
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c(
        "Chi-Square Test of indépendence and Fisher-Freeman-Halton Exact Test I (unpaired)",
        "Mann-Whitney U test 2 (non apparié)",
        "Bhapkar's Test",
        "Wilcoxon test"
      ))
    } else if (explicativeX_var_caractéristique == "catégorielle") {
      return(c(
        # "régression logistique multiniveau (appariée)",
        # "Friedman's ANOVA II (paired)",
        "Chi-Square Test of indépendance en Fisher-Freeman-Halton Exact Test I (unpaired)",
        "Kruskal-Wallis test 1 (explicativeX)"
      ))
    }
  }

  # Pas de test statistique approuvé trouvé. 
  return("Pas de test statistique utile trouvé en lien avec la combinaison de variables X et explicative Y")
}

#Etape 4 : mette en place une fonction pour calculer la normalité 
#Analyser uniquement la normalité d'une variable et adapter le message suivant le résultat. 

get_variable_class_info <- function(variable) {
  if (is.numeric(variable)) {
    # Faire un test shapiro wilk pour effectuer la normalité
    sw_test <- stats::shapiro.test(variable)
    is_normal <- sw_test$p.value >= 0.05

    # Créer un vecteur de character pour sauvegarder le résultat. 
    var_class_info <- character(0)

    if (is_normal) {
      var_class_info <- c(var_class_info, "independante Variable Class: continue (Normally Distributed)")
    } else {
      var_class_info <- c(var_class_info, "independante Variable Class: continue (Not Normally Distributed)")
      # Inclure le test de Shapiro avec une p-value
      shapiro_result <- paste("Shapiro-Wilk Test: p-value =", format(sw_test$p.value, digits = 4))
      var_class_info <- c(var_class_info, shapiro_result)
    }

    return(var_class_info)
  } else {
    # Vérifie si la variable catégorique est binaire ou pas. 
    unique_values <- unique(variable)
    is_binaire <- length(unique_values) == 2

    if (is_binaire) {
      var_class_info <- "explicativeX Variable Class: Categorical (binaire)"
    } else {
      var_class_info <- "explicativeX Variable Class: Categorical (Nominal)"
    }

    return(var_class_info)
  }
}



# Etape 5 : exécuter le test à partir des packages.  Cette fonction va effectuer les tests statistiques à partir de la base de données donné par l'utilisateur. 

#Variables qui ont les différentes données pour les variables. 
perform_statistical_test <- function(data, input) {
  test_name <- input$statistical_test
  expliquéeY_var <- input$expliquéeY_var
  explicativeX_var <- input$explicativeX_var
  identifier_var <- input$identifier_var
  input_mean <- input$input_mean

  # Determiner la valuer de mu basé sur les sélections de l'utilisateur
  #IL est important d'insérer une moyenne hypothétique pour certains tests , notamment ceux ne se basant que sur Y (notamment si "reference value" est mise en place. 
  mu <- if (explicativeX_var == "reference value") input_mean else mean(data[[expliquéeY_var]], na.rm = TRUE)

  # Exécuté les tests 
  #On utilise trycatch qui permet notamment de mettre en place une autre fonction en cas d'erreur
  
  result <- tryCatch(
    {
      switch(test_name,
             "Test du signe" = DescTools::SignTest(x = data[[expliquéeY_var]], mu = mu, alternative = "two.sided"),
             "Wilcoxon test" = stats::wilcox.test(data[[expliquéeY_var]] ~ data[[explicativeX_var]], data, paired = TRUE, alternative = "two.sided"),
             "Mann-Whitney U test 2 (non apparié)" = {
               unique_values <- unique(data[[explicativeX_var]])
               groupee1 <- data[data[[explicativeX_var]] == unique_values[1], expliquéeY_var]
               groupee2 <- data[data[[explicativeX_var]] == unique_values[2], expliquéeY_var]
               stats::wilcox.test(groupee1, groupee2, paired = FALSE, alternative = "two.sided", conf.int = TRUE)
             },
             "Kruskal-Wallis test 1 (explicativeX)" = stats::kruskal.test(data[[expliquéeY_var]] ~ data[[explicativeX_var]], data),
             "Tstudent 1 echantillon" = stats::t.test(data[[expliquéeY_var]], mu = mu, alternative = "two.sided"),
             "T-test (test apparié)" = stats::t.test(data[[expliquéeY_var]] ~ data[[explicativeX_var]], data, paired = TRUE, alternative = "two.sided", var.equal = FALSE),
             "explicativeX samples t-test (non apparié)" = {
               data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
               if (length(levels(data[[explicativeX_var]])) != 2) stop("The explicativeX variable must have exactly two levels for an explicativeX t-test.")
               part_one <- stats::t.test(data[[expliquéeY_var]] ~ data[[explicativeX_var]], data, paired = FALSE, alternative = "two.sided", var.equal = FALSE)

               formula <- stats::as.formula(paste(substitute(expliquéeY_var), "~", substitute(explicativeX_var))) ## formula working example!
               part_two <- rstatix::cohens_d(data, formula,
                                             var.equal = TRUE)

               c(part_one, part_two)

             },
             # "analyse de la variance à mesures répétées (appariée)" = perform_repeated_measures_anova(data, expliquéeY_var, identifier_var, explicativeX_var),
             "One-way ANOVA (unpaired)" = {
               data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
               res.aov <- stats::aov(data[[expliquéeY_var]] ~ data[[explicativeX_var]])
               summary(res.aov)
             },
             "CTest du khi carré d'adéquation" = {
               table_var <- table(data[[expliquéeY_var]], useNA = "no")
               reference_value <- if (explicativeX_var == "reference value") input_mean else max(table_var) / sum(table_var)
               stats::chisq.test(table_var, p = c(1 - reference_value, reference_value))
             },
             " test de McNemar (appareillé)" = { ## Error: Error in exact2x2::exact2x2(groupee_matrix, paired = TRUE, midp = TRUE): 'x' must have at least 2 rows and columns
               unique_values <- unique(data[[explicativeX_var]])
               groupee1 <- data[data[[explicativeX_var]] == unique_values[1], expliquéeY_var]
               groupee2 <- data[data[[explicativeX_var]] == unique_values[2], expliquéeY_var]
               groupee_matrix <- table(groupe1, groupe2)
               exact2x2::exact2x2(groupe_matrix, paired = TRUE, midp = TRUE)
             },#Test sur donnée nominale dichtomoique
             "Test du Chi-carré d'indépendance et test exact de Fisher (non apparié)" = stats::chisq.test(data[[expliquéeY_var]], data[[explicativeX_var]]),
             "Bhapkar's Test" = {
               unique_values <- unique(data[[explicativeX_var]])
               groupee1 <- data[data[[explicativeX_var]] == unique_values[1], expliquéeY_var]
               groupee2 <- data[data[[explicativeX_var]] == unique_values[2], expliquéeY_var]
               irr::bhapkar(cbind(groupe1, groupe2))
             },
             "Test des rangs signés de Wilcoxon II (appareillé" = {
               data[[expliquéeY_var]] <- as.numeric(as.factor(data[[expliquéeY_var]]))
               stats::wilcox.test(data[[expliquéeY_var]], paired = TRUE)
             },
             "Coeff de corrélation de Pearson" = stats::cor(data[[expliquéeY_var]], data[[explicativeX_var]], method = "pearson"),
             "Coeff de corrélation de Spearman" = stats::cor(data[[expliquéeY_var]], data[[explicativeX_var]], method = "spearman"),
             "Cochran's Q Test (paired)" = {
             data[[expliquéeY_var]] <- as.factor(data[[expliquéeY_var]])
             data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
             rstatix::cochran_qtest(data[[expliquéeY_var]] ~ data[[explicativeX_var]] | data[[identifier_var]], data)
             },
             "Test exact de Fischer" = {
               data[[expliquéeY_var]] <- as.factor(data[[expliquéeY_var]])
               data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
               stats::fisher.test(data[[expliquéeY_var]], data[[explicativeX_var]])
             },
             #Partie de tests non paramétrique  : 
             "Test d'ANOVA apparié" = {
              data[[expliquéeY_var]] <- as.numeric(data[[expliquéeY_var]])
             data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
             message(identifier_var)
             formula <- stats::as.formula(paste(substitute(expliquéeY_var), "~", substitute(explicativeX_var), "|", substitute(identifier_var)))
             stats::friedman.test(substitute(expliquéeY_var) ~ substitute(explicativeX_var) | substitute(identifier_var), data)
             stats::friedman.test(formula, data)
             },
             "Friedman's ANOVA II (paired)" = {
             
             
                message(expliquéeY_var)
             
                message(deparse(expliquéeY_var))
                message(substitute(expliquéeY_var))
                message(deparse(substitute(expliquéeY_var)))
             
                perform_friedman_test_now(data, substitute(expliquéeY_var), substitute(explicativeX_var), substitute(identifier_var))
                   data[[expliquéeY_var]] <- as.numeric(data[[expliquéeY_var]])
                   data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
                 stats::friedman.test(data[[expliquéeY_var]] ~ data[[explicativeX_var]] | data[[identifier_var]], data)
               },
                "régression logistique multiniveau (appariée)" = {
                 data[[expliquéeY_var]] <- as.factor(data[[expliquéeY_var]])
                 data[[explicativeX_var]] <- as.factor(data[[explicativeX_var]])
                 message(identifier_var)
                 lme4::glmer(data[[expliquéeY_var]] ~ data[[explicativeX_var]] + (1 | data[[identifier_var]]), data, family = binomial)
              },
             stop(paste0("No appropriate statistical test found for the given combination of expliquéeY and explicativeX variables: ", expliquéeY_var, " and ", explicativeX_var))
      )
    }, #S'il y a erreur , on va alors afficher le code suivant , qui va donc permettre de spécifier le type d'erreur : 
    error = function(e) {
      stop(paste0("Error: ", e))#e : permet de capter le message d'erreur 
      NULL
    }
  )

  return(result)
}

#Créer le serveur 

app_server <- function(input, output, session) {
  shiny::observeEvent(input$launch_modal, {
    datamods::import_modal(
      id = "myid",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data to be used in application"
    )
  })
  
  imported <- datamods::import_server("myid", return_class = "data.frame")
  
  sdata <- shiny::reactive({ #Reactive permet de mettre à jour les données ,notamment la base de données. 
    if (is.null(imported$data())) {#S'il n'y a pas de data set chargé, il charge "mtcars" qui est une base de donnée de R classique. 
      datasets::mtcars
    } else {
      shiny::req(imported$data()) #Si importé=pas 0, on utilise Req permet de vérifier l'absence de données manquantes en plus
      if (nrow(imported$data()) == 0) {  #si après avoir enlever les NA, la base de donnée =0
        datasets::mtcars
      } else {
        imported$data() 
      }
    }
  })
  
  
  
  # Afficher la base de données sous le format de la Datatable. 
  output$dataTable <- DT::renderDataTable({
    shiny::req(sdata())
    DT::datatable(sdata())
  })
  
  # Créer le menu  déroulant pour la variable expliquéeY variable
  output$expliquéeY_var_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    shinyWidgets::pickerInput("expliquéeY_var", "Choisir la variable expliquéeY X", choices = colnames(sdata()))
  })
  
  # Menu déroulant pour la variable explicative X . 
  output$explicativeX_var_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    choices <- c("reference value", colnames(sdata()))
    shinyWidgets::pickerInput(
      "explicativeX_var",
      "Choisir la variable explicativeX ou de référence",
      choices = choices
    )
  })
  
  output$input_mean <- shiny::renderUI({
    shiny::req(sdata())
    shiny::req(input$expliquéeY_var)
    
    ## Récupérer les données des entrées utilisateur.
    data <- sdata()
    
    ## Calculer la moyenne des données 
    if (is.numeric(data[[input$expliquéeY_var]])) {
      reference_value <- mean(data[[input$expliquéeY_var]], na.rm = TRUE)
    } else {
      reference_value <- NULL
    }
    
    ## Seule montrer l'input mean si la valeur de référence est actionnée. 
    #Cela permet d'écrire la valeur de référence qui va permettre notamment de comparer dans certain tests notamment à 1 valeur comme le test du Signe. 
    if (input$explicativeX_var == "reference value") {
      shiny::numericInput("input_mean", "Set reference value", value = reference_value)
    }
  })
  
  # Menu déroulant dynamique qui permet notamment de sélectionner une variable identifiante ID, en fonction des variables explicatives. 
  #cela est surtout utile pour les groupes appareillés ou non (ANOVA)
  #Et cela permet donc de sélectionner les identifants en "doublons".  
  output$identifier_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    shinyWidgets::pickerInput("identifier_var", "Choisir la variable identifiée", choices = colnames(sdata()))
  })
  
  # Menu déroulant des variables expliquéeYs
  output$expliquéeY_var_text <- shiny::renderText({
    shiny::req(input$expliquéeY_var)
    determine_expliquéeY_variable(sdata()[, input$expliquéeY_var])
  })
  
  # Texte additionnel pour la variable explicativeX. 
  output$explicativeX_var_text <- shiny::renderText({
    shiny::req(input$explicativeX_var)
    
    if (input$explicativeX_var %in% colnames(sdata())) {
      determine_explicativeX_variable(sdata()[, input$explicativeX_var])
    }
  })
  
  # Nouveau menu déroulant pour sélectionner le test statistique. 
  output$statistical_test_dropdown <- shiny::renderUI({
    shiny::req(input$expliquéeY_var, input$explicativeX_var)
    
    if (input$explicativeX_var == "reference value") {
      explicativeX_var <- "valeur de référence"
    } else {
      explicativeX_var <- sdata()[, input$explicativeX_var]
    }
    test <- SélectionTestStatistiques(sdata()[, input$expliquéeY_var], explicativeX_var)
    test_options <- c(test)
    shinyWidgets::pickerInput("statistical_test", "Choose statistical test", choices = test_options)
  })
  
  # Afficher l'histogramme de la variable Y sélectionnée. 
  output$expliquéeY_var_histogramme <- shiny::renderPlot({
    shiny::req(sdata(), input$expliquéeY_var)
    créer_expliquéeY_variable_histogramme(sdata()[, input$expliquéeY_var])
  })
  
  # 4.Exporter les tests statistiques

  output$test_report <- shiny::renderPrint({
    result <- perform_statistical_test(sdata(), input)
    if (!is.null(result)) result
  })
}

add_ggplot <- function(geom) {
  gx <- ggplot(data(), aes_string(x = input$expXaxisVar))
  gxy <- ggplot(data(), aes_string(x = input$expXaxisVar, y = input$expYaxisVar))
  switch(geom,
         point = gxy,
         boxplot = gxy,
         histogramme = gx,
         density = gx,
         jitter = gxy
  )
}
add_geom <- function(geom) {
  switch(geom,
         point = geom_point(aes_string(color = input$expColorVar)),
         boxplot = geom_boxplot(aes_string(color = input$expColorVar)),
         histogramme = geom_histogramme(aes_string(color = input$expColorVar)),
         density = geom_density(aes_string(color = input$expColorVar)),
         jitter = geom_jitter(aes_string(color = input$expColorVar))
  )
}


#Lancer l'application 
statistiques <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}

shinyApp(ui = app_ui, server = app_server)