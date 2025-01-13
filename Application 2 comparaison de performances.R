
# Chargement des librairies nécessaires
library(shiny)
library(dplyr)
library(car)       
library(broom)  
library(stringr)  
library(leaps)    #Sélection logistique en régression multiple. 
library(bestglm)   # pour Sélection exhaustive 
library(glmnet)    # pour Elastic Net / Lasso / Ridge

ui <- fluidPage(
  
  titlePanel("Application Shiny : Comparaison de modèles avec sélection exhaustive"),
  
  tabsetPanel(
    
    # Mettre un 1 : Introduction
    tabPanel("Introduction",
             sidebarLayout(
               sidebarPanel(
                 h4("1) Choisir votre jeu de données"),
                 actionButton("useMtcars", "Utiliser mtcars"),
                 br(), br(),
                 fileInput("fileData", "Ou importer un fichier CSV",
                           accept = c(".csv","text/csv","text/comma-separated-values,text/plain")),
                 
                 hr(),
                 h4("2) Choisir la variable Y"),
                 uiOutput("varY_ui_intro"),
                 
                 hr(),
                 h4("3) Sélection de variables X (potentielles)"),
                 uiOutput("varX_ui_intro")
               ),
               
               mainPanel(
                 h3("Bienvenue dans l'application Shiny !"),
                 p("Dans cet Mettre un, vous pouvez :"),
                 tags$ul(
                   tags$li("Utiliser directement le dataset mtcars (intégré dans R),"),
                   tags$li("ou importer votre propre fichier CSV."),
                   tags$li("Sélectionner la variable Y, et cocher/décocher des variables X.")
                 ),
                 p("Les choix ci-dessus seront repris dans les autres Mettre uns.")
               )
             )
    ),
    
    # Mettre un 2 : Résultats automatiques
    tabPanel("Résultats automatiques",
             sidebarLayout(
               sidebarPanel(
                 actionButton("runModels", "Lancer les modèles automatiques"),
                 br(), br(),
                 p("Cliquez pour afficher le VIF (si Y numérique) et les résumés des modèles.")
               ),
               mainPanel(
                 verbatimTextOutput("vifResult"),
                 verbatimTextOutput("modelComparison")
               )
             )
    ),
    
    # Mettre un 3 : Modèle personnalisé
    tabPanel("Modèle personnalisé",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("varX_custom_ui"),
                 actionButton("runCustomModel", "Lancer le modèle personnalisé")
               ),
               mainPanel(
                 verbatimTextOutput("customModelSummary")
               )
             )
    ),
    
    # Mettre un 4 : Sélection exhaustive
    tabPanel("Sélection exhaustive",
             sidebarLayout(
               sidebarPanel(
                 actionButton("runBestSubset", "Calculer Sélection exhaustive"),
                 br(), br(),
                 p("Cliquez pour tester tous les sous-modèles (exhaustif). Aucune sélection finale imposée.")
               ),
               mainPanel(
                 verbatimTextOutput("bestSubsetOutput")
               )
             )
    ),
    
    # Mettre un 5 : Elastic Net
    tabPanel("Elastic Net",
             sidebarLayout(
               sidebarPanel(
                 actionButton("runElasticNet", "Lancer Elastic Net"),
                 br(), br(),
                 p("Cliquez pour effectuer une sélection par Elastic Net (alpha=0.5)"),
                 p("Avec partition : 70% train / 30% test")
               ),
               mainPanel(
                 verbatimTextOutput("elasticNetOutput")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  #  Stocke un data.frame réactif, par défaut = mtcars
  df <- reactiveVal(mtcars)
  
  # Met à jour les inputs quand df() change
  observeEvent(df(), {
    req(df())
    cols <- names(df())
    if(length(cols) == 0) return(NULL)
    
    updateSelectInput(session, "varY_intro",
                      label    = "Variable dépendante (Y):",
                      choices  = cols,
                      selected = cols[1])
    
    updateCheckboxGroupInput(session, "varX_intro",
                             label    = "Variables explicatives (X) potentielles :",
                             choices  = cols,
                             selected = cols[-1])
    
    updateCheckboxGroupInput(session, "varX_custom",
                             label    = "Variables explicatives (X) personnalisées :",
                             choices  = cols,
                             selected = character(0))
  })
  
  #  UI dynamique pour l'Mettre un Intro
  output$varY_ui_intro <- renderUI({
    req(df())
    selectInput("varY_intro", "Variable dépendante (Y):",
                choices = names(df()),
                selected = names(df())[1])
  })
  
  output$varX_ui_intro <- renderUI({
    req(df())
    checkboxGroupInput("varX_intro",
                       "Variables explicatives (X) potentielles :",
                       choices  = names(df()),
                       selected = names(df())[-1])
  })
  
  # UI dynamique pour le modèle personnalisé
  output$varX_custom_ui <- renderUI({
    req(df())
    checkboxGroupInput("varX_custom",
                       "Choisir manuellement vos variables X :",
                       choices = names(df()),
                       selected = character(0))
  })
  
  # Récupère Y et X (de l'intro) pour la suite
  reactiveY <- reactive({
    req(input$varY_intro)
    input$varY_intro
  })
  
  reactiveX <- reactive({
    req(input$varX_intro)
    input$varX_intro
  })
  
  # 6) Bouton "Utiliser mtcars"
  observeEvent(input$useMtcars, {
    df(mtcars)
    showNotification("Vous utilisez maintenant 'mtcars'.")
  })
  
  # 7) Import d'un fichier CSV
  observeEvent(input$fileData, {
    req(input$fileData)
    file_in <- input$fileData
    
    newdata <- tryCatch({
      read.csv(file_in$datapath, header = TRUE, sep = ",")
    }, error = function(e) {
      showNotification("Erreur lors de la lecture du fichier CSV.", type = "error")
      return(NULL)
    })
    
    if(!is.null(newdata)) {
      df(newdata)
      showNotification("Nouveau fichier CSV importé avec succès.")
    }
  })
  
  #  Calcule le VIF (si Y numérique)
  vifResults <- reactive({
    req(reactiveY(), reactiveX())
    y_name  <- reactiveY()
    x_names <- reactiveX()
    
    if(!is.numeric(df()[[y_name]])) {
      return(NULL)
    }
    data_sub <- df()[, c(y_name, x_names), drop=FALSE]
    
    fml <- as.formula(
      paste(y_name, "~", paste(x_names, collapse = " + "))
    )
    mod_temp <- lm(fml, data = data_sub)
    car::vif(mod_temp)
  })
  
  # 9) Mettre un "Résultats automatiques"
  observeEvent(input$runModels, {
    output$vifResult <- renderPrint({
      vals <- vifResults()
      if(is.null(vals)) {
        cat("VIF non calculable : Y n'est pas numérique ou pas de X.\n")
      } else {
        cat("Valeurs du VIF pour chaque variable explicative :\n")
        print(vals)
        cat("\nVariables avec un VIF > 4 :\n")
        print(names(vals[vals > 4]))
      }
    })
    
    output$modelComparison <- renderPrint({
      y_name  <- reactiveY()
      x_names <- reactiveX()
      data_sub <- df()[, c(y_name, x_names), drop=FALSE]
      y_vals   <- df()[[y_name]]
      
      is_binary <- is.factor(y_vals) || is.character(y_vals)
      
      # Régression linéaire si Y numérique
      if(is.numeric(y_vals)) {
        formula_lm <- as.formula(
          paste(y_name, "~", paste(x_names, collapse = "+"))
        )
        model_lm <- lm(formula_lm, data = data_sub)
        
        summary_lm <- summary(model_lm)
        r2_lm      <- summary_lm$r.squared
        fstat_lm   <- summary_lm$fstatistic[1]
        aic_lm     <- AIC(model_lm)
        bic_lm     <- BIC(model_lm)
        
        cat("**Régression linéaire multiple**\n")
        cat("R2       :", r2_lm, "\n")
        cat("F-Stat   :", fstat_lm, "\n")
        cat("AIC      :", aic_lm, "\n")
        cat("BIC      :", bic_lm, "\n\n")
      } else {
        cat("**Régression linéaire multiple** non applicable (Y n'est pas numérique).\n\n")
      }
      
      # Régression logistique si Y binaire
      if(is_binary) {
        y_factor <- factor(y_vals)
        if(length(levels(y_factor)) == 2) {
          formula_glm <- as.formula(
            paste(y_name, "~", paste(x_names, collapse = "+"))
          )
          model_glm <- glm(formula_glm, data = data_sub, family = binomial)
          
          null_glm   <- glm(as.formula(paste(y_name, "~ 1")), 
                            data = data_sub, family = binomial)
          loglik_full <- as.numeric(logLik(model_glm))
          loglik_null <- as.numeric(logLik(null_glm))
          pseudo_r2   <- 1 - (loglik_full / loglik_null)
          
          aic_glm     <- AIC(model_glm)
          bic_glm     <- BIC(model_glm)
          
          cat("**Régression logistique**\n")
          cat("Pseudo R2 (McFadden) :", pseudo_r2, "\n")
          cat("AIC :", aic_glm, "\n")
          cat("BIC :", bic_glm, "\n\n")
        } else {
          cat("**Régression logistique** non applicable : Y n'a pas 2 niveaux.\n\n")
        }
      } else {
        cat("**Régression logistique** non applicable : Y n'est pas binaire.\n\n")
      }
    })
  })
  
  #  Mettre un "Modèle personnalisé"
  observeEvent(input$runCustomModel, {
    output$customModelSummary <- renderPrint({
      req(input$varX_custom, df())
      y_name <- reactiveY()
      x_sel  <- input$varX_custom
      
      data_sub <- df()[, c(y_name, x_sel), drop=FALSE]
      y_vals   <- df()[[y_name]]
      
      if(is.numeric(y_vals)) {
        # LM
        fml <- as.formula(
          paste(y_name, "~", paste(x_sel, collapse = "+"))
        )
        model_custom <- lm(fml, data = data_sub)
        cat("**Modèle linéaire personnalisé :**\n")
        print(summary(model_custom))
        cat("AIC :", AIC(model_custom), "\n")
        cat("BIC :", BIC(model_custom), "\n")
      } else {
        # GLM binomial si Y binaire
        y_factor <- factor(y_vals)
        if(length(levels(y_factor)) == 2) {
          fml <- as.formula(
            paste(y_name, "~", paste(x_sel, collapse = "+"))
          )
          model_custom <- glm(fml, data = data_sub, family = binomial)
          cat("**Modèle logistique personnalisé :**\n")
          print(summary(model_custom))
          cat("\nAIC :", AIC(model_custom), "\n")
          cat("BIC :", BIC(model_custom), "\n")
        } else {
          cat("Impossible de faire un modèle logistique : Y n'a pas 2 niveaux.\n")
        }
      }
    })
  })
  
  # Mettre un "Sélection exhaustive"
  observeEvent(input$runBestSubset, {
    output$bestSubsetOutput <- renderPrint({
      req(reactiveY(), reactiveX(), df())
      
      y_name  <- reactiveY()
      x_names <- reactiveX()
      data_sub <- df()[, c(y_name, x_names), drop=FALSE]
      y_vals   <- df()[[y_name]]
      
      # regsubsets() si Y numérique
      if(is.numeric(y_vals)) {
        form_full <- as.formula(
          paste(y_name, "~", paste(x_names, collapse = "+"))
        )
        
        best_sub <- regsubsets(
          form_full,
          data  = data_sub,
          nbest = 1,
          nvmax = length(x_names),
          method= "exhaustive"
        )
        
        best_sub_sum <- summary(best_sub)
        
        cat("=== Sélection exhaustive (Régression linéaire) ===\n")
        cat("\n- BIC pour chaque taille de modèle :\n")
        print(best_sub_sum$bic)
        
        cat("\n- R2 pour chaque taille de modèle :\n")
        print(best_sub_sum$rsq)
        
        cat("\n- R2 ajusté pour chaque taille de modèle :\n")
        print(best_sub_sum$adjr2)
        
        cat("\n- Tableau which (quelles variables incluses) :\n")
        print(best_sub_sum$which)
        
        cat("\n(Nous n'imposons pas la sélection, nous affichons simplement.)\n")
        
      } else {
        # bestglm() si Y binaire
        y_factor <- factor(y_vals)
        if(length(levels(y_factor)) == 2) {
          Xy <- data.frame(Y = y_factor, data_sub[, x_names, drop=FALSE])
          
          best_logit <- bestglm(
            Xy     = Xy,
            family = binomial,
            IC     = "AIC",
            method = "exhaustive"
          )
          
          cat("=== Sélection exhaustive (Régression logistique) ===\n\n")
          cat("Tableau récapitulatif de tous les modèles testés :\n")
          print(best_logit$Subsets)
          
          cat("\nModèle \"meilleur\" d'après AIC :\n")
          print(summary(best_logit$BestModel))
          
          cat("\n(Nous n'imposons pas la sélection, nous affichons simplement.)\n")
          
        } else {
          cat("Y n'est pas binaire => Sélection exhaustive logistique non applicable.\n")
        }
      }
    })
  })
  
  # Mettre un "Elastic Net" : avec partition train=70%, test=30%, R2adj et F-stat si Y est numérique
  observeEvent(input$runElasticNet, {
    output$elasticNetOutput <- renderPrint({
      req(reactiveY(), reactiveX(), df())
      
      y_name  <- reactiveY()
      x_names <- reactiveX()
      data_sub <- df()[, c(y_name, x_names), drop=FALSE]
      y_vals   <- df()[[y_name]]
      
      is_binary <- is.factor(y_vals) || is.character(y_vals)
      
      # CAS 1 : Y numérique => régression
      if(is.numeric(y_vals)) {
        
        set.seed(123)
        n <- nrow(data_sub)
        train_index <- sample(seq_len(n), size = 0.7 * n)  # 70% pour train
        
        data_train <- data_sub[train_index, , drop=FALSE]
        data_test  <- data_sub[-train_index, , drop=FALSE]
        
        # Prépare matrice X + Y sur TRAIN
        form_full <- as.formula(
          paste(y_name, "~", paste(x_names, collapse = "+"))
        )
        x_mat_train <- model.matrix(form_full, data_train)[, -1]
        y_train     <- data_train[[y_name]]
        
        # Entraîne Elastic Net (alpha=0.5) en "gaussian"
        cvfit <- cv.glmnet(x_mat_train, y_train, alpha=0.5, family="gaussian")
        
        cat("=== Elastic Net (Y numérique) ===\n\n")
        cat("Lambda min :", cvfit$lambda.min, "\n")
        cat("Lambda 1se :", cvfit$lambda.1se, "\n\n")
        
        # Coeffs à lambda.min
        coefs <- coef(cvfit, s="lambda.min")
        cat("Coefficients (lambda.min) :\n")
        print(coefs)
        
        # Prédiction sur TEST
        x_mat_test <- model.matrix(form_full, data_test)[, -1]
        y_test     <- data_test[[y_name]]
        
        y_pred     <- predict(cvfit, newx=x_mat_test, s="lambda.min")
        
        # Calcul de R2, R2 ajusté et F-stat sur TEST
        SSE <- sum((y_test - y_pred)^2)
        SST <- sum((y_test - mean(y_test))^2)
        R2  <- 1 - SSE/SST
        
        n_test <- length(y_test)
        # Nombre de coeffs non nuls (hors intercept)
        nb_nonzero <- sum(coefs != 0) - 1
        if(nb_nonzero < 1) nb_nonzero <- 1
        
        # R2 ajusté
        R2_adj <- 1 - (1 - R2) * ((n_test - 1)/(n_test - nb_nonzero - 1))
        
        # F-stat approximatif
        num   <- R2 / nb_nonzero
        den   <- (1 - R2) / (n_test - nb_nonzero - 1)
        F_stat <- num/den
        
        cat("\nSur le set de test (30% des données) :\n")
        cat("R2          :", round(R2, 4), "\n")
        cat("R2 ajusté   :", round(R2_adj, 4), "\n")
        cat("F-statistic :", round(F_stat, 4), "\n")
        
        # CAS 2 : Y binaire => logistic (pas de R2adj ni F-stat)
      } else if(is_binary) {
        y_factor <- factor(y_vals)
        if(length(levels(y_factor)) == 2) {
          
          cat("=== Elastic Net (Y binaire) ===\n")
          cat("(Pas de R2 ajusté ni F-stat pour la logistique.)\n\n")
          
          set.seed(123)
          n <- nrow(data_sub)
          train_index <- sample(seq_len(n), size = 0.7*n)
          
          data_train <- data_sub[train_index, , drop=FALSE]
          data_test  <- data_sub[-train_index, , drop=FALSE]
          
          form_full <- as.formula(
            paste(y_name, "~", paste(x_names, collapse = "+"))
          )
          x_mat_train <- model.matrix(form_full, data_train)[, -1]
          y_train     <- factor(data_train[[y_name]])
          
          cvfit <- cv.glmnet(x_mat_train, y_train, alpha=0.5, family="binomial")
          
          cat("Lambda min :", cvfit$lambda.min, "\n")
          cat("Lambda 1se :", cvfit$lambda.1se, "\n\n")
          
          coefs <- coef(cvfit, s="lambda.min")
          cat("Coefficients (lambda.min) :\n")
          print(coefs)
          
          cat("\nPour la logistique, vous pouvez calculer AUC, accuracy, etc.\n")
          
        } else {
          cat("Y n'a pas exactement 2 modalités => logistique non applicable.\n")
        }
        
      } else {
        cat("Y n'est pas numérique ni binaire => Elastic Net non applicable.\n")
      }
    })
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)

