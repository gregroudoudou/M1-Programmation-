#Techiques qui servent lorsqu'on NA PAS DE MODELE FIXE !  !!! 
#MODEL FIXE = stationnarité
#modele aléatoire= filtres ici 

#exemple : la logistique.../les méanges... 

#Lissage: =séries plus complexes à composantes inob servables. 
#Pas de besoin de STATIONNARISER ! : ici il suffit d'extraire 
#les différentes composantes : les coefficients évoluent dans le temps 
#Tout bouge ! 


#alors que la stationnarité signifie que les COEFFICIENTS 
#sont CONSTANTS

#On va utiliser un algorithme : on modélise la DYNAMIQUE des composantes
#INOBSERVABLES: le MODELE EVOLUE DANS LE TEMPS! 

N <- 500
a <- 1
l <- 0.01
rho <- 0.9
##G�n�ration d'une variable al�atoire suivant une loi normale
set.seed(246810)
v <- ts(rnorm(N,0,1))
plot(v)

#Bruit blanc : processus stationnaire avec une ACF=0,1 mais SANS DERIVEF : 
y <- ts(rep(0,N))#ts : time series #tirage de 0 à N
for (t in 2:N){
  y[t]<- 0.1*y[t-1]+v[t]
} #de T à N : il faut traduire. 
plot(y,type='l', ylab="0,1*y[t-1]+v[t]")#Amodifier suivant le rho
abline(h=0)
acf(y)

acf(y)

#EMarche aléatoire
y <- ts(rep(0,N))#ts : time series #tirage de 0 à N
for (t in 2:N){
  y[t]<- 0.9*y[t-1]+v[t] #modifions....
} #de T à N : il faut traduire. 
plot(y,type='l', ylab="y[t-1]+v[t]")#Amodifier suivant le rho
abline(h=0)
acf(y)

#Ce n'est pas cyclique ici
#Un cycle est déterminée par une croissance monotone
#

#Exemple de série non stationnaire. 
y <- ts(rep(0,N))#ts : time series #tirage de 0 à N
for (t in 2:N){
  y[t]<- 0.9*y[t-1]+v[t] #modifions....
} #de T à N : il faut traduire. 
plot(y,type='l', ylab="0,9*y[t-1]+v[t]")#Amodifier suivant le rho
abline(h=0)
acf(y)





##processus stationnaire: yt=rho yt-1+et
y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- a+rho*y[t-1]+v[t]
}
plot(y,type='l', ylab="a+rho*y[t-1]+v[t]")
abline(h=0)

#####tendance d�terministe plus d�rive
a<-1 #au départ 
y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- a+l*time(y)[t]+rho*y[t-1]+v[t]
}
plot(y,type='l', ylab="a+l*time(y)[t]+rho*y[t-1]+v[t]")
abline(h=0)
#On combien tendance déterministe et stochastique ici : 
#c'est le PIB ! 


acf(y)
#Il est non stationnaire dès le départ. !
#Rho 0=gamma(hà/gamma(0) qui est toujours
#Il y a une stationnarité en variance 


#####tendance d�terministe sans dérive 

y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- a+l*time(y)[t]+v[t]
}
plot(y,type='l', ylab="a+l*time(y)[t]+rho*y[t-1]+v[t]")
abline(h=0)

#####une tendance stochastique : une série financière 
#Pour 
y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- y[t-1]+v[t]
}
plot(y,type='l', ylab="y[t-1]+v[t]")
abline(h=0)
acf(y)
#Ici on peut voir qu'il y a une stationnarité en variance. 
#Il y a des dynamiques avec une variance qui n'est pas stable
#mais 

#####Tendance stochastique avec une d�rive
a <- 0.1
y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- a+y[t-1]+v[t]
}
plot(y,type='l', ylab="a+y[t-1]+v[t]")
abline(h=0)
acf(y)

######Tendance d�terministe
y <- ts(rep(0,N))
for (t in 2:N){
  y[t]<- a+l*time(y)[t]+y[t-1]+v[t]
}
plot(y,type='l', ylab="a+l*time(y)[t]+y[t-1]+v[t]")
abline(h=0)

####L'idée est qu'on va faire une décomposition et une recomposition; 

library(fpp3)
library(fpp2)
library(forecast)
oildata <- window(oil, start=1996)

# Estimate parameters: lisssage exponentielle
fc <- ses(oildata, h=5)
summary(fc[["model"]])

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")


window(ausair, start=1990, end=2004) %>%#crée une sous liste à partir de cela
  holt(h=5, PI=FALSE) %>% #l'ajoute dans la fonction suivante ! 
  autoplot() #On fait toujours l'autoplot après rien car on a deja FAIT LA FONCTION AVANT ! : 

library(forecast)
library(ggplot2)

# Charger une série temporelle
data <- AirPassengers
ts_data <- ts(data, start = c(1949, 1), frequency = 12)

# Modèle de Holt (prédictions 12 mois)
holt_model <- holt(ts_data, h=12)

# Affichage avec autoplot()
autoplot(holt_model)


library(forecast)
library(ggplot2)

# Création d'une série temporelle factice avec une tendance rampante
set.seed(123) #permet l'aléatoire ! 
time_series <- ts(cumsum(rnorm(100, mean = 0.5, sd = 0.2)), start = c(2000), frequency = 1)

# Application du modèle de Holt avec tendance
holt_model <- holt(time_series, h = 10, damped=TRUE)
#Rappel :TRUE toujours en majuscule ! 

# Affichage des prévisions
autoplot(holt_model) +
  ggtitle("Prévision avec tendance rampante (modèle de Holt)") +
  xlab("Années") + ylab("Valeurs prédites") +
  theme_minimal()
#Autoplot permet d'afficher les valeurs RRECEDENTES 
#valeurs prédites

#SAISONNALITE : additive et multiplicative. 
#Multiplicatif : on doit notamment penser à faire un model multiplicatif

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

data <- AirPassengers  # Exemple avec une série classique


##Afficher  GRAPHIQUEMENT les différentes composantes temporeslles :une SAISON additive
#Tendances : /saisons / 
ts_data <- ts(data, start = c(1949, 1), frequency = 12)

# 1er etae : DECOMPOSE : Décomposition additive : à effectuer en 1er 
decomp_add <- decompose(ts_data, type = "additive")

# 2eme étape : Affichage des composantes
autoplot(decomp_add)

# 2eme étape : Décomposition multiplicative
decomp_mult <- decompose(ts_data, type = "multiplicative")

# Affichage des composantes
autoplot(decomp_mult)


###HOLT WINT : hw  : Faire les modeles de prédiction de saisonnalité ADDITIVE et multiplicative 
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

#Utiliser ggplot2 notamment pour faire une comparaison 
<"entre les deux modeles additifs et multplicatifs. "
library(ggplot2)
library(forecast)

autoplot(aust) +
  autolayer(fit1, series="Holt-Winters Additif", PI=FALSE) +
  autolayer(fit2, series="Holt-Winters Multiplicatif", PI=FALSE) +
  ggtitle("Comparaison des modèles Holt-Winters") +
  ylab("Nombre de touristes") + xlab("Année") +
  scale_color_manual(values=c("blue", "red")) +
  theme_minimal()

#Métriques
accuracy(fit1)
accuracy(fit2)
#On voit que le modèle  de fit2 


#1: test de l'application du logarithme afin de présenter une saisonnalité multiplicative
#ON doit donc passer au LOGARITHME des deux côtés; 
# Simuler des données
set.seed(123)
X1 <- runif(100, 1, 10)
X2 <- runif(100, 1, 10)
Y_add <- 5 + 2 * X1 + 3 * X2 + rnorm(100, 0, 1)  # Additif
Y_mult <- 5 * X1^2 * X2^3 * exp(rnorm(100, 0, 0.1))  # Multiplicatif

# Transformation logarithmique
par(mfrow=c(1,2))
plot(X1, Y_mult, main="Avant log", xlab="X1", ylab="Y")
plot(log(X1), log(Y_mult), main="Après log", xlab="log(X1)", ylab="log(Y)")

#Comme on observe que le Logartihme permet d'avoir une tendance plus linéaire
#Cela suggère une relation multiplicative ! 

# Charger les packages nécessaires
library(forecast)

# Charger une série temporelle (exemple avec AirPassengers, qui est multiplicatif)
data(AirPassengers)
ts_data <- AirPassengers

# 1. Visualisation des variations saisonnières
plot(ts_data, main="Série temporelle", ylab="Nombre de passagers", xlab="Temps")

# 2. Décomposition de la série temporelle
decomp_add <- decompose(ts_data, type="additive")
decomp_mult <- decompose(ts_data, type="multiplicative")

# 3. Ajustement des modèles Holt-Winters
hw_add <- HoltWinters(ts_data, seasonal="additive")
hw_mult <- HoltWinters(ts_data, seasonal="multiplicative")

# Comparaison des erreurs de prévision
rmse_add <- sqrt(mean(hw_add$fitted[, "xhat"] - ts_data)^2)
rmse_mult <- sqrt(mean(hw_mult$fitted[, "xhat"] - ts_data)^2)

#Utiliser CAT : notamment pour trouver les AIC : 
cat("RMSE modèle additif:", rmse_add, "\n")
cat("RMSE modèle multiplicatif:", rmse_mult, "\n")

# Vérification avec ets() (modèles exponentiels généralisés)
ets_add <- ets(ts_data, model="AAA")  # Tendance, saisonnalité et erreur additives
ets_mult <- ets(ts_data, model="MMM")  # Tendance, saisonnalité et erreur multiplicatives

# Comparaison des AIC
AIC(ets_add, ets_mult)

#Ici le modèle multiplicatif apparaît minimiser le BIC, ce qui signife
#Cela signifie qu'il est donc performant ! 




##Filtre de KALMAN : 
# Installer les packages si nécessaire
if (!require(forecast)) install.packages("forecast", dependencies=TRUE)
if (!require(KFAS)) install.packages("KFAS", dependencies=TRUE)

# Charger les packages
library(forecast)
library(KFAS)

# 🔹 1. Création d'une série temporelle simulée
set.seed(42)
n <- 120  # 10 ans de données mensuelles
time <- 1:n
trend <- 0.05 * time
seasonality <- 2 * sin(2 * pi * time / 12)
noise <- rnorm(n, mean = 0, sd = 0.5)

# Génération des séries temporelles
ts_data <- ts(10 + trend + seasonality + noise, start = c(2000, 1), frequency = 12)

# 🔹 2. Ajustement des modèles Holt-Winters
hw_standard <- holt(ts_data, h=12, damped=FALSE)  # Tendance persistante
hw_damped <- holt(ts_data, h=12, damped=TRUE)     # Tendance amortie

# 🔹 3. Ajustement du Filtre de Kalman (Tendance locale)
kalman_model <- SSModel(ts_data ~ SSMtrend(degree=2, Q=list(matrix(NA), matrix(NA))), H=NA)
kalman_fit <- fitSSM(kalman_model, inits=c(0.1, 0.1, 0.1))$model
kalman_smoothed <- KFS(kalman_fit, smoothing="state")$alphahat[,1]

# 🔹 4. Calcul des erreurs RMSE
rmse_hw_standard <- sqrt(mean((fitted(hw_standard) - ts_data)^2))
rmse_hw_damped <- sqrt(mean((fitted(hw_damped) - ts_data)^2))
rmse_kalman <- sqrt(mean((kalman_smoothed - ts_data)^2))

# 🔹 5. Visualisation des résultats
plot(ts_data, col="black", lwd=2, main="Comparaison Holt-Winters & Filtre de Kalman", ylab="Valeur", xlab="Temps")
lines(fitted(hw_standard), col="blue", lty=2, lwd=2)
lines(fitted(hw_damped), col="red", lty=2, lwd=2)
lines(kalman_smoothed, col="green", lty=2, lwd=2)
legend("topleft", legend=c("Série Réelle", "Holt-Winters Standard", "Holt-Winters Dampé", "Filtre de Kalman"), 
       col=c("black", "blue", "red", "green"), lty=c(1,2,2,2), lwd=2)

# 🔹 6. Affichage des erreurs
cat("RMSE Holt-Winters Standard:", rmse_hw_standard, "\n")
cat("RMSE Holt-Winters Dampé:", rmse_hw_damped, "\n")
cat("RMSE Filtre de Kalman:", rmse_kalman, "\n")

#Le filtre de Kalman semble donc beaucoup plus adapté si la série a une 
#tendance avec ses erreurs
# on abandonne l'hypothèse de bruit blanc, donc stationnaire en niveau. 


###Exerice sur H02 : à développer ! 

#On calcule le modèle ETS : avec le filtre de Kalman . 
h02 %>% ets() %>% autoplot()

h02 %>% ets() %>% accuracy()

#Modèle sans  niveau lente : exponentiel. 
h02 %>% ets(model="AAA", damped=FALSE) %>% accuracy()
#Modèle avec niveau et qui tend lentement vers son équilibre de long-terme ! 
h02 %>% ets(model="AAA", damped=FALSE) %>% accuracy()

#Division entre le train /et le test : avec WINDOWS. 
train <-window(h02, end=c(2004,12))
test <-window(h02, start=2005) 
#a fonction window(h02, end=c(2004,12)) en R extrait une sous-série de h02, en conservant uniquement les observations jusqu'en décembre 2004.
#Ici on divise entre le train et le test. 
fit1 <-ets(train)
fit2 <-ets(test, model = fit1)

#Commande accuracy : évaluation du modèle. Avoir les métriques d'une série temporelle : RMSE/ME...
accuracy(fit2)
#Cela doit aller vers 0. 
#Mais ici on a surtout diviser 


###Holt winter
#Calculer pour une hypothese de modele sans tendance ni saisonnalité. 
exponentiel

library(forecast)
# Lissage exponentiel simple sans STATIONNARITE :  ! 
ses_model <- ses(h02, h=12)  # Prévisions sur 12 périodes
autoplot(ses_model)


#Calculer holt
holt<-holt(h02)

holtwintermodel<-hw (h02)
accuracy(holtwintermodel)
#ON adonc une erreur bien plus importante  ! 

#Saisonnalité additive ou multplicative 
hwadd<-hw (h02,method="additive")

hwmul <-hw (h02, method="multiplicative")

#Rampante ou pas 
hwadd<-hw (h02,method="additive")

#On va utiliser un algorithme : on modélise la DYNAMIQUE des composantes
#INOBSERVABLES: le MODELE EVOLUE DANS LE TEMPS!
#Pour causale : on va supposer que Xt contient une COMPOSANTE SAISONNIERE et faire le NECESSAIRE
#Et donc il faudrait DABORD PREDIRE XT, XT CHAPEUA par LISSAGE!
#Donc PRENDRE CETTE COMPLEXITE est TOUJOURS MEILLEUR !

#Comparer : quel est le modele le plus COMPATIBLE avec les DONEEES ! 



