

rm(list=ls())


# Constantes, Fonctions et Librairies ----

# Librairies
# __________

if (system.file(package='tidyverse')=="") { install.packages("tidyverse") }
if (system.file(package='corrplot')=="") { install.packages("corrplot") }
if (system.file(package='ROCR')=="") { install.packages("ROCR") }

library(tidyverse)
library(corrplot)
library(ROCR)




# couleurs
# ________

col_pluie.demain <- c("orange", "deepskyblue4")

# Renommage des colonnes : tableau de correspondances
# ______________________

col_rename= rbind ( c("Month", "month"),
                c("Temperature.daily.mean..2.m.above.gnd.", "temperature.mean"),
                c("Relative.Humidity.daily.mean..2.m.above.gnd.", "humidity.mean"),
                c("Mean.Sea.Level.Pressure.daily.mean..MSL.", "pressure.mean"),
                c("Total.Precipitation.daily.sum..sfc.", "precipitation"),
                c("Snowfall.amount.raw.daily.sum..sfc.", "snowfall"),
                c("Total.Cloud.Cover.daily.mean..sfc.", "total.cloud.mean"),
                c("High.Cloud.Cover.daily.mean..high.cld.lay.","high.cloud.mean"),
                c("Medium.Cloud.Cover.daily.mean..mid.cld.lay.", "med.cloud.mean"),
                c("Low.Cloud.Cover.daily.mean..low.cld.lay.", "low.cloud.mean"),
                c("Sunshine.Duration.daily.sum..sfc.", "sunshine"),
                c("Shortwave.Radiation.daily.sum..sfc.", "radiation"),
                c("Wind.Speed.daily.mean..10.m.above.gnd.", "wind.speed.mean.10"),
                c("Wind.Direction.daily.mean..10.m.above.gnd.", "wind.dir.10"),
                c("Wind.Speed.daily.mean..80.m.above.gnd.", "wind.speed.mean.80"),
                c("Wind.Direction.daily.mean..80.m.above.gnd.", "wind.dir.80"),
                c("Wind.Speed.daily.mean..900.mb.", "wind.speed.mean.900"),
                c("Wind.Direction.daily.mean..900.mb.","wind.dir.900"),
                c("Wind.Gust.daily.mean..sfc.","wind.gust.mean"),
                c("Temperature.daily.max..2.m.above.gnd.","temperature.max"),
                c("Temperature.daily.min..2.m.above.gnd.", "temperature.min"),
                c("Relative.Humidity.daily.max..2.m.above.gnd.","humidity.max"),
                c("Relative.Humidity.daily.min..2.m.above.gnd.", "humidity.min"),
                c("Mean.Sea.Level.Pressure.daily.max..MSL.", "pressure.max"),
                c("Mean.Sea.Level.Pressure.daily.min..MSL.", "pressure.min"),
                c("Total.Cloud.Cover.daily.max..sfc.","total.cloud.max"),
                c("Total.Cloud.Cover.daily.min..sfc.","total.cloud.min"),
                c("High.Cloud.Cover.daily.max..high.cld.lay.", "high.cloud.max"),
                c("High.Cloud.Cover.daily.min..high.cld.lay.", "high.cloud.min"),
                c("Medium.Cloud.Cover.daily.max..mid.cld.lay.", "med.cloud.max"),
                c("Medium.Cloud.Cover.daily.min..mid.cld.lay.", "med.cloud.min"),
                c("Low.Cloud.Cover.daily.max..low.cld.lay.","low.cloud.max"),
                c("Low.Cloud.Cover.daily.min..low.cld.lay.", "low.cloud.min"),
                c("Wind.Speed.daily.max..10.m.above.gnd.","wind.speed.max.10"),
                c("Wind.Speed.daily.min..10.m.above.gnd.", "wind.speed.min.10"),
                c("Wind.Speed.daily.max..80.m.above.gnd.", "wind.speed.max.80"),
                c("Wind.Speed.daily.min..80.m.above.gnd.", "wind.speed.min.80"),
                c("Wind.Speed.daily.max..900.mb.","wind.speed.max.900"),
                c("Wind.Speed.daily.min..900.mb.", "wind.speed.min.900"),
                c("Wind.Gust.daily.max..sfc.","wind.gust.max"),
                c("Wind.Gust.daily.min..sfc.", "wind.gust.min"))

colnames(col_rename)<- c("original", "new")


# Préparation du Data Frame en vue de la modélisation
# ___________________________________

process_data <- function(fun_data)
{
  # Retrait des colonnes inutiles : X, Year, Hour, Minutes
  fun_data <- fun_data[,!(names(fun_data) %in% c("X", "Year", "Day", "Hour", "Minute"))]
  
  # Month : passage en type factoriel
  fun_data$Month <- as.factor(fun_data$Month)
  summary(fun_data)
  
  # Renommage des colonnes selon le tableau de correspondances
  for (c in col_rename[,"original"]) 
  {
    i <- which(colnames(fun_data)==c)
    colnames(fun_data)[i]<-col_rename[i,"new"]
    
    print(paste("renaming:", col_rename[i,"original"], " => ", col_rename[i,"new"]))
  }
  
  # ajout des colonnes 'amplitude' en fin de tableau pour les variables "mean/min/max"
  fun_data <- cbind(fun_data, 
                    temperature.amplitude=fun_data$temperature.max - fun_data$temperature.min)

  fun_data <- cbind(fun_data,
                    humidity.amplitude=fun_data$humidity.max - fun_data$humidity.min)

  fun_data <- cbind(fun_data,
                    pressure.amplitude=fun_data$pressure.max - fun_data$pressure.min)

  fun_data <- cbind(fun_data,
                    total.cloud.amplitude=fun_data$total.cloud.max - fun_data$total.cloud.min)

  fun_data <- cbind(fun_data,
                    low.cloud.amplitude=fun_data$low.cloud.max - fun_data$low.cloud.min)

  fun_data <- cbind(fun_data,
                    med.cloud.amplitude=fun_data$med.cloud.max - fun_data$med.cloud.min)

  fun_data <- cbind(fun_data,
                    high.cloud.amplitude=fun_data$high.cloud.max - fun_data$high.cloud.min)

  fun_data <- cbind(fun_data,
                    wind.speed.amplitude.10=fun_data$wind.speed.max.10 - fun_data$wind.speed.min.10)

  fun_data <- cbind(fun_data,
                    wind.speed.amplitude.80=fun_data$wind.speed.max.80 - fun_data$wind.speed.min.80)

  fun_data <- cbind(fun_data,
                    wind.speed.amplitude.900=fun_data$wind.speed.max.900 - fun_data$wind.speed.min.900)

  fun_data <- cbind(fun_data,
                    wind.gust.amplitude=fun_data$wind.gust.max - fun_data$wind.gust.min)

  # ajout de variables booléennes en fin de tableau : precipitation, snowfall, sunshine
  fun_data$precipitation_bool <- (fun_data$precipitation>0.1)
  fun_data$snowfall_bool <- (fun_data$snowfall>0)
  fun_data$sunshine_bool <- (fun_data$sunshine>0)
  
  return (fun_data)
}

# Boxplot des variables mean/min/max/amplitude selon la variable pluie.demain
# ___________________________________

display_boxplot_with_pluiedemain <- function(fun_mean, fun_min, fun_max, fun_pluie.demain, fun_main="")
{
  fun_amplitude=fun_max-fun_min
  
  par(mfrow=c(2,2))
  
  # moyenne
  boxplot(fun_mean ~ fun_pluie.demain, col=col_pluie.demain,
          main=paste(fun_main), cex.main=0.9,
          ylab="mean",
          xlab="pluie.demain")

  # min
  boxplot(fun_min ~ fun_pluie.demain, col=col_pluie.demain,
          main=paste(fun_main), cex.main=0.9,
          ylab="min",
          xlab="pluie.demain")
  
  # max
  boxplot(fun_max ~ fun_pluie.demain, col=col_pluie.demain,
          main=paste(fun_main), cex.main=0.9,
          ylab="max",
          xlab="pluie.demain")
  
  # amplitude
  boxplot(fun_amplitude ~ fun_pluie.demain, col=col_pluie.demain,
          main=paste(fun_main), cex.main=0.9,
          ylab="amplitude (max-min)",
          xlab="pluie.demain")
  
  
  par(mfrow=c(1,1))
  
}



# Visualisation des prédictions versus valeurs réelles
#
# fun_res_predict : vecteur des prédictions P(pluie.demain=1)
# fun_seuil : seuil de décision
# _____________________________________________________________

display_TN_TP <- function (fun_res_predict, fun_seuil) {
  
  couleur <- rep("darkolivegreen4", length(fun_res_predict))
  couleur [ (fun_res_predict>fun_seuil) != (dat.meteo.train$pluie.demain[!scp.train]==1) ] <- "firebrick"
  
  plot(fun_res_predict, dat.meteo.train$pluie.demain[!scp.train], 
       main="prédictions versus valeurs réelles",
       xlab="probabilité estimée",
       ylab="valeur réelle de pluie.demain",
       col=couleur,
       pch=19)
  
  # tracé du seuil 
  abline(v=fun_seuil, lty=2)
  
  legend(0.6, 0.8, 
         legend=c("Prédiction correcte", "FP ou FN", paste("seuil : ", fun_seuil)),
         pch=c(19, 19, 3),
         col=c("darkolivegreen4", "firebrick", "black"))
  
}

# Visualisation de la courbe ROC
# ______________________________
#
# fun_res_predict : vecteur des prédictions P(pluie.demain=1)
#
# sortie
# p : prediction
#
display_ROC <- function(fun_res_predict) {
  
  p <- prediction(fun_res_predict, dat.meteo.train$pluie.demain[!scp.train])
  perf <- performance(p, "tpr", "fpr")
  plot(performance(p, "tpr", "fpr"),
       main="courbe ROC")
  
  abline(a=0, b=1)
  
  return (p)
}


# Calcul du seuil de prédiction optimal sur critère "taux de classification" (TP+TN/N+P)
# __________________________________________________________________________
#
# fun_res_predict : vecteur des prédictions P(pluie.demain=1)
#
compute_opt_threshold <- function (fun_res_predict) {
  
  seuils = seq(0, 1, by=.01)
  
  # On teste chaque seuil et on trouve le seuil qui minimise le coût global.
  
  scores_accuracy <- sapply (seuils, FUN=function(s) {
    
    y_pred <- (fun_res_predict >= s)
    y_TP_TN <- (y_pred==(dat.meteo.train$pluie.demain[!scp.train]==TRUE))
    
    return(mean(y_TP_TN))
  })
  
  # Seuil dont le score (Taux de classification) est le plus fort
  current.model.seuil_opt <- seuils[which.max(scores_accuracy)]
  return (current.model.seuil_opt)
}

# Calcul de la précision de prédiction (ratio TP + TN / P + N)
# ____________________________________
#
# fun_prediction : vecteur des valeurs Y prédites (TRUE/FALSE)
#

compute_precision <- function(fun_prediction) {
  
  # Bonne prédictions
  TP_TN <- fun_prediction==(dat.meteo.train$pluie.demain[!scp.train]==TRUE)
  
  # Précision (TP+TN)/(N+P)
  # return (length(TP_TN[TP_TN==TRUE]) / length(TP_TN))
  return (mean(TP_TN))
  
}

# Calcul de l'AUC (Area Under the Curve)
# ______________________________________
#
# fun_p : prediction issus de l'affichage de la courbe ROC
#

compute_auc <- function (fun_p) {
  
  auc <- performance(fun_p, "auc")
  
  return(auc@y.values[1])
}

# ANALYSE de la DEVIANCE d'un MODELE de REGRESSION LOGISTIQUE
# ___________________________________________________________

compute_deviance_analysis <- function(fun_res_glm) {
  
  # Rappel du résultat de la régression
  summary(fun_res_glm)
  
  # n (nombre d'échantillon) et k (nombre de covariables) 
  print(paste("Nombre d'échantillons n : ", nrow(fun_res_glm$data)))
  print(paste("Nombre de covariables : ", length(fun_res_glm$coefficients)))
  
  # Déviance résiduelle Dk : Mk versus Msat
  Dk <- fun_res_glm$deviance
  print(paste("Déviance résiduelle Dk (Mk vs Msat) : ", Dk))
  Dk.df <- fun_res_glm$df.residual # degrés de liberté = n - (k + 1)
  print(paste("Déviance résiduelle Dk (degrés de liberté n-k-1) : ", Dk.df))
  
  # Déviance nulle D0 : M0 versus Msat
  D0 <- fun_res_glm$null.deviance
  print(paste("Déviance nulle D0 (M0 vs MSat) : ", D0))
  D0.df <- fun_res_glm$df.null # degrés de liberté de la déviance nulle ()
  print(paste("Déviance nulle D0 (degrés de liberté n)  : ", D0.df))
  
  # Test M0 versus Mk (D0 - Dk) : 
  chitest_D0_Dk <- pchisq(D0-Dk, D0.df-Dk.df, lower.tail = FALSE)
  print(paste("Test M0 versus Mk (D0-Dk) : p-valeur=", chitest_D0_Dk))
  
  
  # p-valeur < 5%, on rejette le modèle M0 ; le modèle Mk explique mieux les données que le modèle constant
  # p-valeur > 5%, on NE rejette PAS le modèle M0 ; les covariables n'expliquent pas mieux les données observées qu'un modèle constant 
  
  # Test Mk versus Msat (Dk)
  chitest_Dk <- pchisq(Dk, Dk.df, lower.tail = FALSE) 
  print(paste("Test Mk versus Msat (Dk) : p-valeur=", chitest_Dk))
  
  
  # p-valeur < 5% : On rejette H0:Mk, le modèle n'est pas suffisamment riche pour expliquer mieux les données observées qu'un modèle saturé (pi sans structure)
  # p-valeur > 5% : On NE rejette PAS H0:Mk, le modèle Mk a capturé suffisamment de variabilité pour expliquer les données mieux qu'un modèle saturé (pi sans structure)
  
  # res <- c(test.M0.vs.Mk=chitest_D0_Dk, test.Mk.vs.Msat=chitest_Dk)
  res <- c(RESULTS_DEV_M0_VS_MK=chitest_D0_Dk, RESULTS_DEV_MK_VS_MSAT=chitest_Dk)
  
  return(res)
}

# GESTION du TABLEAU RECAPITULATIF des MODELES ETUDIES
# _____________________

RESULTS <- data.frame()
RESULTS_FORMULA <- "formula"
RESULTS_NB_COVARIABLES <- "nb.covariables"
RESULTS_NB_COEFF <- "nb.coefficients"
RESULTS_AIC <- "aic"
RESULTS_DEV_MK_VS_MSAT <- "deviance.test.mk.versus.msat"
RESULTS_DEV_M0_VS_MK <- "deviance.test.m0.versus.mk"
RESULTS_current.model.seuil_opt <- "seuil.optimal"
RESULTS_PRECISION <- "precision"
RESULTS_AUC <- "auc"
RESULTS_ERREUR <- "erreur"

results_initialize <- function () {
  
  local_results <- data.frame(
                    formula=NA,
                    nb.covariables=NA,
                    nb.coefficients=NA,
                    aic=NA,
                    deviance.test.mk.versus.msat=NA,
                    deviance.test.m0.versus.mk=NA,
                    seuil.optimal=NA,
                    precision=NA,
                    auc=NA,
                    erreur=NA)
  
  return (local_results)
}

results_create_model <- function (fun_results, fun_model_name) {

  # initialisation de la table si besoin
  if (ncol(fun_results)==0) {

    results <- results_initialize()
  }
  else {

    # Ajout d'une ligne à la variable globale pour le modèle à créer

    results <- rbind( fun_results, rep(NA,ncol(fun_results)))
  }

  row.names(results)[nrow(results)] <- fun_model_name

  return(results)
}

results_set_values <- function (fun_results, fun_model_name, fun_model_glm) {
  
  # formule du modèle
  fun_results[fun_model_name, RESULTS_FORMULA] <- deparse1(as.formula(fun_model_glm))
                                                       
  # nombre de covariables du modèle
  fun_results[fun_model_name, RESULTS_NB_COVARIABLES] <- ncol(fun_model_glm$model)-1 # on retire pluie.demain
  
  # nombre de coefficients
  fun_results[fun_model_name, RESULTS_NB_COEFF] <- length(fun_model_glm$coefficients)
  
  # aic
  fun_results[fun_model_name, RESULTS_AIC] <- fun_model_glm$aic
  

  return(fun_results)
}

#__________________________________________________________________________
#
# PREPARATION des DONNEES ----
#
#__________________________________________________________________________

## Chargement des données ----

## Chargement du jeu de données d'entraînement
dat.meteo.train.raw <- read.table("meteo.train.csv", 
                    sep=",", dec=".",
                    header = 1,
                    na.strings="NA",
                    colClasses = c(rep('numeric', 46), 'logical'))

ncol(dat.meteo.train.raw)
colnames(dat.meteo.train.raw)
nrow(dat.meteo.train.raw)

## Chargement du jeu de données de test
dat.meteo.test.raw <- read.table("meteo.test.csv", 
                                 sep=",", dec=".",
                                 header = 1,
                                 na.strings="NA",
                                 colClasses = c(rep('numeric', 46)))


ncol(dat.meteo.test.raw)
colnames(dat.meteo.test.raw)
nrow(dat.meteo.test.raw)


  # 47 colonnes pour meteo.train.raw / 46 colonnes pour meteo.test.raw
  #
  # 6 colonnes descriptives NUMERIQUES : id, Year, Month, Day, Hour, Minute
  # 40 variables NUMERIQUES : conditions météorologiques
  # (train) 1 colonne BOOL : variable à expliquer 'pluie.demain' (booleenne) : TRUE/FALSE
  

## Préparation des données ----

# Processing des données : 
# . retrait des colonnes descriptives
# . renommage des colonnes
# . ajout de colonnes (amplitudes et booleens)

dat.meteo.train<-process_data(dat.meteo.train.raw)
dat.meteo.test<-process_data(dat.meteo.test.raw)

  # Après transformation:
  #
  # 56 colonnes pour meteo.train / 55 colonnes pour meteo.test
  #
  # 1 covariable FACTORIELLE : month
  # 51 covariables NUMERIQUES : conditions météorologiques
  # 3 covariables BOOLEENNES : precipitation_bool / snowfall_bool / sunshine_bool
  # (train) 1 colonne BOOL : variable à expliquer 'pluie.demain' (booleenne) : TRUE/FALSE


# contrôle des dimensions et des types

summary(dat.meteo.train)
summary(dat.meteo.test)

ncol(dat.meteo.train)
sort(colnames(dat.meteo.train))
nrow(dat.meteo.train)

ncol(dat.meteo.test)
sort(colnames(dat.meteo.test))
nrow(dat.meteo.test)

#__________________________________________________________________________
#
# ANALYSE EXPLORATOIRE ----
#
#__________________________________________________________________________

par(mfrow=c(1,1))

## Variable d'intérêt pluie.demain ----

table(dat.meteo.train$pluie.demain)
barplot(table(dat.meteo.train$pluie.demain), col=col_pluie.demain)

  # La répartition des jours de pluie et des jours où il ne pleut pas est équilibrée
  # Pas de problème de représentativité de l'une ou l'autre des valeurs


## Analyse des covariables Mean/Min/Max----

### Température ----

var_main="Température"

# Distribution selon la variable d'intéret

var_mean=dat.meteo.train$temperature.mean
var_min=dat.meteo.train$temperature.min
var_max=dat.meteo.train$temperature.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

  # TEMPERATURES
  #
  # => pluie.demain==TRUE si Températures plus hautes et amplitudes plus faibles
  # => pluie.demain==FALSE si Températures plus basses et amplitudes plus grandes
  #
  # => Les corrélations entre min/max/mean sont fortes (>0.9)
  # => Les corrélations entre min/max/mean et amplitude sont plutôt faîble. 
  # => La corrélation la plus faible est constatée entre min et amplitude (~0.21)
  #
  # Idées pour la modélisation 
  # . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
  # . inclure l'amplitude 
  # . Considérer la covariable produit amplitude*min


### Humidité relative ----

var_main="Humidité"

# Distribution selon la variable d'intéret

var_mean=dat.meteo.train$humidity.mean
var_min=dat.meteo.train$humidity.min
var_max=dat.meteo.train$humidity.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

  # HUMIDITE
  #
  # => La variation de la distribution de l'humidité selon pluie.demain est minime
  #
  # => pluie.demain==TRUE : une légère tendance à une humidité moyenne/min/max plus grande
  # => pluie.demain==FALSE : une légère tendance à une humidité moyenne/min/max plus faible
  #
  # => Les corrélations entre min/max/mean sont positives et relativement fortes (>0.75)
  # => La corrélation entre max et amplitude est faîble.
  #
  # Idées pour la modélisation 
  # . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : max en l'occurrence
  # . inclure l'amplitude 
  # . Considérer la covariable produit amplitude*max

  
### Pression ----

var_main="Pression"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$pressure.mean
var_min=dat.meteo.train$pressure.min
var_max=dat.meteo.train$pressure.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

  # PRESSION
  #
  #
  # => pluie.demain==TRUE : pressions moyenne/min/max plus faibles / amplitude plus grande
  #
  # => Les corrélations entre min/max/mean sont fortes
  # => Les corrélations entre min/max/mean et amplitude sont faibles 
  # => La corrélation la plus faible est constatée entre amplitude et max
  #
  # Idées pour la modélisation 
  # . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : max en l'occurrence
  # . inclure l'amplitude 
  # . Considérer la covariable produit amplitude*max



### Nébulosité totale ----

var_main="Nébulosité totale"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$total.cloud.mean
var_min=dat.meteo.train$total.cloud.min
var_max=dat.meteo.train$total.cloud.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Distribution des valeurs min/max/mean

hist(var_mean,
     main=var_main)
quantile(var_mean)

hist(var_min)
quantile(var_min) # valeurs principalement 0

hist(var_max)
quantile(var_max) # Valeurs principalement 100%

hist(var_max-var_min)
quantile(var_max-var_min) # Valeurs principalement 0 et 100%, peu de valeurs intermédiaires

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# NEBULOSITE TOTALE
#
# . mean/min/max sont corrélés
# . amplitude est moyennement corrélé avec min/max
# . amplitude est peu corrélé avec mean
# . amplitude présente les caractéristiques suivantes
#   . amplitude est "bipolarisée" : soit 0% (min et max sont les même valeurs) soit 100% (min et max sont 0/100)
#
# Idées pour la modélisation 
# . inclure mean et amplitude
# . inclure le produit mean*amplitude
#


### Nébulosité basse ----

var_main="Nébulosité basse"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$low.cloud.mean
var_min=dat.meteo.train$low.cloud.min
var_max=dat.meteo.train$low.cloud.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Distribution des valeurs min/max/mean

hist(var_mean,
     main=var_main)
quantile(var_mean)

hist(var_min)
quantile(var_min) # valeurs principalement 0

hist(var_max)
quantile(var_max) # Valeurs principalement 100%

hist(var_max-var_min)
quantile(var_max-var_min) # Valeurs principalement 0 et 100%, peu de valeurs intermédiaires

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# NEBULOSITE BASSE
#
# . min/max/mean sont corrélées
#
# Idées pour la modélisation 
# . inclure min et amplitude
# . inclure le produit min*amplitude

### Nébulosité médium ----

var_main="Nébulosité médium"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$med.cloud.mean
var_min=dat.meteo.train$med.cloud.min
var_max=dat.meteo.train$med.cloud.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)


# Distribution des valeurs min/max/mean

hist(var_mean,
     main=var_main)
quantile(var_mean)

hist(var_min)
quantile(var_min) # valeurs principalement 0

hist(var_max)
quantile(var_max) # Valeurs principalement 100% / 50% des données sont 100%

hist(var_max-var_min)
quantile(var_max-var_min) # Valeurs principalement 0 et 100%, peu de valeurs intermédiaires

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# NEBULOSITE MEDIUM
#
# Idées pour la modélisation 
# . inclure min et amplitude
# . inclure le produit min*amplitude


### Nébulosité haute ----

var_main="Nébulosité haute"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$high.cloud.mean
var_min=dat.meteo.train$high.cloud.min
var_max=dat.meteo.train$high.cloud.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Distribution des valeurs min/max/mean

hist(var_mean,
     main=var_main)
quantile(var_mean)

hist(var_min)
quantile(var_min) # valeurs principalement 0

hist(var_max)
quantile(var_max) # Valeurs principalement 100% / 50% des données sont 100%

hist(var_max-var_min)
quantile(var_max-var_min) # Valeurs principalement 0 et 100%, peu de valeurs intermédiaires
quantile(var_max[dat.meteo.train$pluie.demain]-var_min[dat.meteo.train$pluie.demain]) # Quantile quand pluie.demain=TRUE
quantile(var_max[!dat.meteo.train$pluie.demain]-var_min[!dat.meteo.train$pluie.demain]) # Quantile quand pluie.demain=FALSE

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

  # NEBULOSITE HAUTE
  #
  # Idées pour la modélisation 
  # . inclure min et amplitude sous forme booleenne
  # . inclure le produit min*amplitude

### Corrélation entre nébulosités ----

# corrélation entre les moyennes

attach(dat.meteo.train)
cloud.mean <- cbind(low.cloud.mean, med.cloud.mean, high.cloud.mean)
cor(cbind(cloud.mean))
corrplot(cor(cloud.mean), title=var_main)
detach(dat.meteo.train)

# corrélation entre les min

attach(dat.meteo.train)
cloud.min <- cbind(low.cloud.min, med.cloud.min, high.cloud.min)
cor(cbind(cloud.min))
corrplot(cor(cloud.min), title=var_main)
detach(dat.meteo.train)

# corrélation entre les max

attach(dat.meteo.train)
cloud.max <- cbind(low.cloud.max, med.cloud.max, high.cloud.max)
cor(cbind(cloud.max))
corrplot(cor(cloud.max), title=var_main)
detach(dat.meteo.train)

# corrélation entre les amplitudes (max-min)

attach(dat.meteo.train)
cloud.amplitude <- cbind(low.cloud.max-low.cloud.min, med.cloud.max-med.cloud.min, high.cloud.max-high.cloud.min)
cor(cbind(cloud.amplitude))
corrplot(cor(cloud.amplitude), title=var_main)
detach(dat.meteo.train)

  # CORRELATION entre NEBULOSITES
  #
  # entre nébulosités basse et haute : corrélations positives et faibles entre mean/min/max/amplitude (~0.1/0.2)
  # 
  # entre nébulosités basse et moyenne : corrélations positives et relativement faibles entre mean/min/max/amplitude (~0.3/0.5)
  #
  # entre nébulosités moyenne et haute : corrélations positives et relativement forte entre mean/min/max/amplitude (~0.6)
  #
  # Idées pour la modélisation 
  # . corrélations raisonnables entre nébulosités
  # . inclure le schéma des 3 nébulosités sous la forme imaginée
  
### Vitesse et sens du vent à 10 m (force et direction) ----

#### Vitesse du vent ----

var_main="Vitesse du Vent à 10 m"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$wind.speed.mean.10
var_min=dat.meteo.train$wind.speed.min.10
var_max=dat.meteo.train$wind.speed.max.10

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# VENT à 10 mètre
#
# => Les corrélations entre min/max/mean sont fortes
# => La corrélation la plus faible est constatée entre amplitude et min
#
# Idées pour la modélisation 
# . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
# . inclure l'amplitude 
# . Considérer la covariable produit amplitude*min
#

#### Sens du vent ----

var_main="Sens du Vent à 10 m"

# Distribution selon la variable d'intérêt

var_total=dat.meteo.train$wind.dir.10

# histogramme des valeurs
hist(var_total)
hist(var_total[dat.meteo.train$pluie.demain]) # pluie.demain==TRUE
hist(var_total[!dat.meteo.train$pluie.demain]) # pluie.demain==FALSE


# Distribution selon la pluie demain

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main,
        ylab=var_main,
        xlab="pluie.demain")

# Quantiles

length(var_total[!dat.meteo.train$pluie.demain])
quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE

length(var_total[dat.meteo.train$pluie.demain])
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE

  # SENS du VENT à 10 mètre
  #
  # Il semble y avoir une
  #
  # Idées pour la modélisation 
  # . inclure le sens du vent à 10 mètres


### Vitesse et sens du vent à 80 m (force et direction) ----

#### Vitesse du vent ----

var_main="Vitesse du Vent à 80 m"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$wind.speed.mean.80
var_min=dat.meteo.train$wind.speed.min.80
var_max=dat.meteo.train$wind.speed.max.80

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# VENT à 80 mètres
#
# => Les corrélations entre min/max/mean sont fortes
# => Les corrélations entre min/max/mean et amplitude sont positives et 
#     . forte avec mean et max (>0.8)
#     . relativement forte avec mean (~0.6)
#     . relativement faible avec min (~0.6)
#
# Idées pour la modélisation 
# . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
# . inclure l'amplitude 
# . Considérer la covariable produit amplitude*min
#


#### Sens du vent ----

var_main="Sens du Vent à 80 m"

# Distribution selon la variable d'intérêt

var_total=dat.meteo.train$wind.dir.80

# histogramme des valeurs
hist(var_total)

# Distribution selon la pluie demain

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main,
        ylab=var_main,
        xlab="pluie.demain")

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE


  # VENT à 80 mètres
  #
  # => Les corrélations entre min/max/mean sont positives et relativement fortes (>0.8)
  # => Les corrélations entre min/max/mean et amplitude sont positives et 
  #     . forte avec mean et max (>0.8)
  #     . relativement forte avec mean (~0.6)
  #     . relativement faible avec min (~0.6)
  #
  # Idées pour la modélisation 
  # . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
  # . inclure l'amplitude 
  # . Considérer la covariable produit amplitude*min
  #

### Vitesse et sens du vent à 900 m (force et direction) ----

#### Vitesse du vent ----

var_main="Vitesse du Vent à 900 m"

# Distribution selon la variable d'intérêt

var_mean=dat.meteo.train$wind.speed.mean.900
var_min=dat.meteo.train$wind.speed.min.900
var_max=dat.meteo.train$wind.speed.max.900

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

# VENT à 900 mètres
#
# => Les corrélations entre min/max/mean sont positives et relativement fortes (>0.9)
# => Les corrélations entre min/max/mean et amplitude sont positives et 
#     . forte avec max
#     . relativement forte avec mean
#     . relativement faible avec min
#
# Idées pour la modélisation 
# . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
# . inclure l'amplitude 
# . Considérer la covariable produit amplitude*min
#


#### Sens du vent ----

var_main="Sens du Vent à 900 m"

# Distribution selon la variable d'intérêt

var_total=dat.meteo.train$wind.dir.900

# histogramme des valeurs
hist(var_total)

# Distribution selon la pluie demain

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main,
        ylab=var_main,
        xlab="pluie.demain")

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE


# SENS du VENT à 900 mètres
#
# => on note une différence notable dans la distribution des directions du vent 
# en fonction du fait qu'il ait plu le lendemain
#
# Idées pour la modélisation 
# . inclure la direction du vent
# . combiner à la donnée vitesse du vent
# . i.e amplitude*min*dir
#

### Corrélation entre vitesses et sens du vent ----

# corrélation entre les moyennes

var_main="vitesses moyennes 10/80/900"

attach(dat.meteo.train)
speed.mean <- cbind(wind.speed.mean.10, wind.speed.mean.80, wind.speed.mean.900)
cor(cbind(speed.mean))
corrplot(cor(speed.mean), title=var_main)
detach(dat.meteo.train)

  # Tres forte corrélation des moyennes 10/80/900 (> 80%)
  # on choisira une seule variable parmi 10/80/900

# corrélation entre les min

attach(dat.meteo.train)
speed.min <- cbind(wind.speed.min.10, wind.speed.min.80, wind.speed.min.900)
cor(cbind(speed.min))
corrplot(cor(speed.min), title=var_main)
detach(dat.meteo.train)

  # Tres forte corrélation des min 10/80 (> 90%)
  # Relativement forte corrélation des min 10/900 et 80/900 (> 65%)
  # on choisira une seule variable parmi 10/80/900


# corrélation entre les max

attach(dat.meteo.train)
speed.max <- cbind(wind.speed.max.10, wind.speed.max.80, wind.speed.max.900)
cor(cbind(speed.max))
corrplot(cor(speed.max), title=var_main)
detach(dat.meteo.train)

  # Tres forte corrélation des max 10/80 (> 90%)
  # Relativement forte corrélation des max 10/900 et 80/900 (> 75%)
  # on choisira une seule variable parmi 10/80/900

# corrélation entre les amplitudes (max-min)

attach(dat.meteo.train)
speed.amplitude <- cbind(wind.speed.amplitude.10=wind.speed.max.10-wind.speed.min.10, 
                         wind.speed.amplitude.80=wind.speed.max.80-wind.speed.min.80, 
                         wind.speed.amplitude.900=wind.speed.max.900-wind.speed.min.900)
cor(cbind(speed.amplitude))
corrplot(cor(speed.amplitude), title=var_main)
detach(dat.meteo.train)

  # Tres forte corrélation des amplitudes 10/80 (> 87%)
  # Relativement forte corrélation des min 10/900 et 80/900 (> 60%)
  # on choisira une seule variable parmi 10/80/900

# corrélation des amplitudes avec la variable d'intérêt (numérique pour l'occasion)

attach(dat.meteo.train)
wind_vs_pluiedemain <- cbind(pluie.demain, speed.amplitude)
cor(wind_vs_pluiedemain)[,1]
corrplot(cor(wind_vs_pluiedemain), title=var_main)
detach(dat.meteo.train)

  # Les corrélations entre les amplitudes 10/80/900 
  # et la variable d'intéret projetée sur [0,1]
  # sont faibles (~25%).
  #
  # Le vent du jour a peu d'influence sur le risque de pluie du lendemain

  # CORRELATION entre VENTS
  #
  # . Les données à 10, 80, 900 sont fortement corrélées
  # . Les données à 900 sont celles les plus corrélées avec la variable pluie.demain projetée sur [0;1]


### Rafales de vent ----

var_main="Rafales de vent"

# Distribution selon la variable d'intéret

var_mean=dat.meteo.train$wind.gust.mean
var_min=dat.meteo.train$wind.gust.min
var_max=dat.meteo.train$wind.gust.max

display_boxplot_with_pluiedemain(var_mean, var_min, var_max, dat.meteo.train$pluie.demain, var_main)

# Corrélation entre covariables

cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min))
corrplot(cor(cbind(var_mean, var_min, var_max, amplitude=var_max-var_min)),
         title=var_main)

  # RAFALES de VENT
  #
  # => Les corrélations entre min/max/mean sont fortes (>0.8
  # => Les corrélations entre max/mean et amplitude sont plutôt forte (>0.6)
  # => La corrélation entre min et amplitude est faible (0.15)
  #
  # Idées pour la modélisation 
  # . Inclure un unique représentant parmi moyenne/min/max fortement corrélées : min en l'occurrence
  # . inclure l'amplitude 
  # . Considérer la covariable produit amplitude*min
  

## Analyse des covariables simples ----

### Précipitations ----

var_main="Précipitations"

var_total=dat.meteo.train$precipitation

# histogramme des valeurs

hist(var_total)
length(var_total[var_total==0]) / length(var_total[])

  # => La majorité des valeurs est nulle : pas de pluie aujourd'hui

# Lien entre la variable booléenne selon la pluie demain

mosaicplot( (var_total!=0) ~ dat.meteo.train$pluie.demain, 
            col=col_pluie.demain,
            main=var_main,
            ylab="pluie demain",
            xlab="Précipitations aujourd'hui (prec!=0)")

  # => Quand il a plu, le risque de pluie le lendemain est plus fort
  # => Quand il n'a pas plu, le risque de pluie le lendemain est plus faible

# Distribution

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main="Ensoleillement")

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE

  # => Quand il a plu le lendemain, 75% des valeurs de précipitations sont 0
  # => Quand il n'a pas plu le lendemain, 50% des valeurs sont 0

  # PRECIPITATION
  #
  # Différence dans les distributions selon pluie.demain
  #
  # La majorité des valeurs sont nulles
  # . pluie.demain=TRUE : 50% des valeurs sont nulles
  # . pluie.demain=FALSE : 75% des valeurs sont nulles
  #
  # Idée pour la modélisation 
  # . Inclure Précipitation 
  # . Inclure Précipitation sous forme booléenne
  # . inclure précipitation et la variable booléenne (précipitation>0.1)



### Enneigement ----

var_main="Enneigement"

var_total=dat.meteo.train$snowfall

# histogramme des valeurs d'enneigement

hist(var_total)
hist(var_total[dat.meteo.train$pluie.demain])
hist(var_total[!dat.meteo.train$pluie.demain])
length(var_total[var_total==0]) / length(var_total[])

  # => 95% des valeurs sont nulles : pas de neige

# Relation entre l'enneigement et la pluie demain

mosaicplot( (var_total!=0) ~ dat.meteo.train$pluie.demain, 
           col=col_pluie.demain,
           main=var_main,
           ylab="pluie demain",
           xlab="Neige aujourd'hui")

  # => légère différence entre le ratio de "pluie.demain" selon l'enneigement du jour.  
  # Quand il a neigé, le risque de pluie est plus fort le lendemain
  # quand il n'a pas neigé, 1 chance sur 2 qu'il pleuve le lendemain

# Distribution

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main)

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE

  # => Qu'il pleuve ou non le lendemain, 75% des valeurs sont nuls
  # => Quand il a neigé la veille, le risque de pluie est plus fort à mesure que les précipitations de neige sont fortes
  

  # NEIGE du JOUR :
  #
  # Idée pour la modélisation 
  # . Inclure Neige (snowfall) 
  # . Inclure Neige -snowfall_bool) sous forme booléenne (snowfall>0)


### Ensoleillement ----

var_main="Ensoleillement"

var_total=dat.meteo.train$sunshine

# histogramme des valeurs d'ensoleillement

hist(var_total)
length(var_total[var_total==0]) / length(var_total[])
quantile(var_total, 0.1)

  # => 10% des valeurs sont nulles (10% des journées sans soleil)
  

# Relation entre l'enneigement et la pluie demain

mosaicplot( (var_total!=0) ~ dat.meteo.train$pluie.demain, 
            col=col_pluie.demain,
            ylab="pluie demain",
            xlab="Ensoleillement aujourd'hui")

  # => On note une différence dans le risque de pluie selon que la journée a été ensoleillée ou non
  # => Quand la journée n'a pas été ensoleillée (=0) : le risque de pluie est plus grand


# Distribution

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main)

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE


# ENSOLEILLEMENT :
#
# Idée pour la modélisation 
# . Inclure Ensoleillement 
# . Inclure Ensoleillement sous forme booléenne (sunshine>0)


### Rayonnement ----

var_main="Rayonnement"

var_total=dat.meteo.train$radiation

# Distribution

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main)

# histogramme des valeurs d'ensoleillement

hist(var_total)

# Distribution selon pluie.demain

boxplot(var_total ~ dat.meteo.train$pluie.demain, col=col_pluie.demain,
        main=var_main,
        xlab="pluie demain",
        ylab=var_main)

  # => Légère différence de la distribution de la radiation selon pluie.demain
  # => Un rayonnement plus faible augmente le risque de pluie

# Quantiles

quantile(var_total[dat.meteo.train$pluie.demain==FALSE]) # quantile des précipitations quand pluie.demain==FALSE
quantile(var_total[dat.meteo.train$pluie.demain==TRUE]) # quantile des précipitations quand pluie.demain==TRUE


# RAYONNEMENT :
#
# => La variation de la distribution du rayonnement selon pluie.demain est minime
#
# => pluie.demain==TRUE : une légère tendance à un rayonnement plus faible
#
# Idées pour la modélisation 
# . inclure le rayonnement en l'état


### Mois (catégorielle) ----

# Lien entre le mois et le risque de pluie

mosaicplot(factor(dat.meteo.train.raw$Month) ~ dat.meteo.train$pluie.demain, 
           col=col_pluie.demain,
           ylab="pluie demain",
           xlab="Mois")

  # Mois 
  #
  # => Le mois de l'année a une influence sur la possibilité de pluie le lendemain
  #
  # Idée pour la modélisation
  #. Inclure le mois sous forme catégorielle

## Colinéarité entre familles de covariables ----

# vérifier la corrélation 
# de toutes les variables
# des variables sélectionnées

# corrélation sur le jeu de données d'entrainement
dat.meteo.train %>% 
  select(-month) %>% 
  cor() %>% 
  corrplot(title = "Jeu meteo.train - Corrélation entre covariables",
           tl.cex=0.5)

# corrélation sur le jeu de données de test
dat.meteo.test %>% 
  select(-month) %>% 
  cor() %>% 
  corrplot(title = "Jeu meteo.test - Corrélation entre covariables",
           tl.cex=0.5)

  # les schémas de corrélation sont identiques dans les 2 jeux de données
  # Pas de contrindications à utiliser un modèle entraîné et validé sur meteo.train 
  # sur meteo.test

#__________________________________________________________________________
#
# ENTRAINEMENT des MODELES CANDIDATS ----
#
#__________________________________________________________________________

## Jeu d'entraînement / jeu de validation ----

# CROSS-VALIDATION : 
#
# 20% de valeurs dans le jeu Train : entraînement des modèles
# 80% de valeurs dans le jeu Train : mesure de précision des modèles
#
scp.train.size <- 0.8 # 0..1 : part des données dédiée à l'entraînement

if (file.exists("ref.training_validation.rdata")) {
  
    load(file="ref.training_validation.rdata") # Si une sauvegarde de la répartition train/test existe déjà , on la récupère
}

if (!exists("scp.train")) {
## if (is.na(scp.train)==TRUE) { # sinon on la crée et on la sauvegarde
  
    # générer le vecteur train 80 / test 20
    scp.train = sample(c(TRUE, FALSE), nrow(dat.meteo.train), replace=TRUE, prob=c(scp.train.size, 1-scp.train.size))
    
    # enregistrer le vecteur pour un prochain usage
    save(scp.train, file="ref.training_validation.rdata")
}

length(scp.train)
length(scp.train[scp.train==TRUE])
length(scp.train[scp.train==FALSE])
nrow(dat.meteo.train)


## STRATEGIE 1 : utilisation des variables initiales ----

### Modèle complet (toutes variables initiales) ----

s1.res.glm.0.formula <- formula("pluie.demain ~ .")

s1.res.glm.0 <- glm(s1.res.glm.0.formula, 
                       data=dat.meteo.train[scp.train,1:42],
                       family="binomial")

summary(s1.res.glm.0)

# store results
RESULTS <- results_create_model(RESULTS, expression(s1.res.glm.0))
RESULTS <- results_set_values(RESULTS, "s1.res.glm.0", s1.res.glm.0)

# Modèle 1

# s1.res.glm.1 <- glm(pluie.demain ~ ., 
#                        data=dat.meteo.train[scp.train,1:42],
#                        family=binomial)
# 
# summary(s1.res.glm.1)

# Modèle 2

# s1.res.glm.2 <- glm(pluie.demain ~ . - 
#                            month -
#                            wind.gust.max -
#                            wind.gust.min -
#                            low.cloud.max,
#                          data=dat.meteo.train[scp.train,1:42],
#                          family=binomial)
# 
# summary(s1.res.glm.2)



### STEP forward ----

# modèle initial = model constant
s1.model.constant <- glm (pluie.demain~1,data=dat.meteo.train[scp.train,1:42], family=binomial)
s1.model.full <- formula(s1.res.glm.0)

# Sélection des covariables par step forward
s1.res.step_forward <- step (s1.model.constant, scope=s1.model.full, direction="forward")
summary(s1.res.step_forward)

# store results
RESULTS <- results_create_model(RESULTS, expression(s1.res.step_forward))
RESULTS <- results_set_values(RESULTS, "s1.res.step_forward", s1.res.step_forward)


### STEP both from constant model ----

# Sélection des covariables par step both from a constant model
s1.res.step_both_from_constant <- step (s1.model.constant, scope=s1.model.full, direction="both")
summary(s1.res.step_both_from_constant)

# store results
RESULTS <- results_create_model(RESULTS, expression(s1.res.step_both_from_constant))
RESULTS <- results_set_values(RESULTS, "s1.res.step_both_from_constant", s1.res.step_both_from_constant)

### STEP backward ----

# Sélection des covariables par step backward 
s1.res.step_backward <- step (s1.res.glm.0, direction="backward")
summary(s1.res.step_backward)

# store results
RESULTS <- results_create_model(RESULTS, "s1.res.step_backward")
RESULTS <- results_set_values(RESULTS, "s1.res.step_backward", s1.res.step_backward)

### STEP both from full-model ----

# Sélection des covariables par step both from the biggest model
s1.res.step_both_from_full <- step (s1.res.glm.0, direction="both")
summary(s1.res.step_both_from_full)

# store results
RESULTS <- results_create_model(RESULTS, "s1.res.step_both_from_full")
RESULTS <- results_set_values(RESULTS, "s1.res.step_both_from_full", s1.res.step_both_from_full)


## STRATEGIE 2 : Modèles issus de l'analyse exploratoire ----

#### Modèle complet ----

# Modèle issu de l'analyse exploratoire

# Modèle selected.0 : toutes les covariables identifiées comme pertinentes pas l'analyse exploratoire
s2.res.glm.0 <- glm(pluie.demain ~ 
                              
                              # factors
                              month +
                              
                              # single-values with mean/min/max/amplitude
                              temperature.amplitude*temperature.min +
                              humidity.amplitude*humidity.max +
                              pressure.amplitude*pressure.max +
                              
                              # cloud
                              total.cloud.amplitude*total.cloud.mean +
                              low.cloud.amplitude*low.cloud.min +
                              med.cloud.amplitude*med.cloud.min +
                              high.cloud.amplitude*high.cloud.min +

                              # wind
                              wind.speed.amplitude.80*wind.speed.min.80*wind.dir.80 +
                              wind.speed.amplitude.900*wind.speed.min.900*wind.dir.900 +
                              wind.gust.amplitude*wind.gust.min +

                              # others
                              precipitation +
                              precipitation_bool +
                              snowfall +
                              snowfall_bool +
                              sunshine +
                              sunshine_bool +
                              radiation,

                            data=dat.meteo.train[scp.train,],
                            family=binomial,
                            na.action=na.exclude)

summary(s2.res.glm.0)

# store results
RESULTS <- results_create_model(RESULTS, "s2.res.glm.0")
RESULTS <- results_set_values(RESULTS, "s2.res.glm.0", s2.res.glm.0)


### STEP forward ----

# modèle initial = model constant
s2.model.constant <- glm (pluie.demain~1,data=dat.meteo.train[scp.train,], family=binomial)
s2.model.full <- formula(s2.res.glm.0)

# Sélection des covariables par step forward
s2.res.step_forward <- step (s2.model.constant, scope=s2.model.full, data=dat.meteo.train[scp.train,], direction="forward")
summary(s2.res.step_forward)

# store results
RESULTS <- results_create_model(RESULTS, "s2.res.step_forward")
RESULTS <- results_set_values(RESULTS, "s2.res.step_forward", s2.res.step_forward)


### STEP both from constant model ----

# Sélection des covariables par step both from a constant model
s2.res.step_both_from_constant <- step (s2.model.constant, scope=s2.model.full, direction="both")
summary(s2.res.step_both_from_constant)

# store results
RESULTS <- results_create_model(RESULTS, "s2.res.step_both_from_constant")
RESULTS <- results_set_values(RESULTS, "s2.res.step_both_from_constant", s2.res.step_both_from_constant)

### STEP backward ----

# Sélection des covariables par step backward 
s2.res.step_backward <- step (s2.res.glm.0, direction="backward")
summary(s2.res.step_backward)

# store results
RESULTS <- results_create_model(RESULTS, "s2.res.step_backward")
RESULTS <- results_set_values(RESULTS, "s2.res.step_backward", s2.res.step_backward)

### STEP both from full-model ----

# Sélection des covariables par step both from the biggest model
s2.res.step_both_from_full <- step (s2.res.glm.0, direction="both")
summary(s2.res.step_both_from_full)

# store results
RESULTS <- results_create_model(RESULTS, "s2.res.step_both_from_full")
RESULTS <- results_set_values(RESULTS, "s2.res.step_both_from_full", s2.res.step_both_from_full)

#__________________________________________________________________________
#
# VALIDATION des MODELES ----
#
#__________________________________________________________________________

## Analyse de la déviance ----

### Stratégie 1 ----

print("Stratégie 1 : modèle complet")
dev <- compute_deviance_analysis(s1.res.glm.0)

RESULTS["s1.res.glm.0", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s1.res.glm.0", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("Stratégie 1 : step forward")
dev <- compute_deviance_analysis(s1.res.step_forward)

RESULTS["s1.res.step_forward", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s1.res.step_forward", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("Stratégie 1 : step both from constant model")
dev <- compute_deviance_analysis(s1.res.step_both_from_constant)

RESULTS["s1.res.step_both_from_constant", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s1.res.step_both_from_constant", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("Stratégie 1 : step backward")
dev <- compute_deviance_analysis(s1.res.step_backward)

RESULTS["s1.res.step_backward", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s1.res.step_backward", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("Stratégie 1 : step both from full model")
dev <- compute_deviance_analysis(s1.res.step_both_from_full)

RESULTS["s1.res.step_both_from_full", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s1.res.step_both_from_full", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

  # . Les modèles font tous mieux qu'un modèle constant et un modèle saturé

### Stratégie 2 ----

print("<Modèle 'Variables sélectionnées'> complet")
dev <- compute_deviance_analysis(s2.res.glm.0)

RESULTS["s2.res.glm.0", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s2.res.glm.0", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("<Modèle 'Variables sélectionnées' step forward>")
dev <- compute_deviance_analysis(s2.res.step_forward)

RESULTS["s2.res.step_forward", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s2.res.step_forward", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("<Modèle 'Variables sélectionnées' step both from constant model>")
dev <- compute_deviance_analysis(s2.res.step_both_from_constant)

RESULTS["s2.res.step_both_from_constant", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s2.res.step_both_from_constant", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("<Modèle 'Variables sélectionnées' step backward>")
dev <- compute_deviance_analysis(s2.res.step_backward)

RESULTS["s2.res.step_backward", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s2.res.step_backward", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]

print("<Modèle 'Variables sélectionnées' step both from full>")
dev <- compute_deviance_analysis(s2.res.step_both_from_full)

RESULTS["s2.res.step_both_from_full", RESULTS_DEV_M0_VS_MK] <- dev["RESULTS_DEV_M0_VS_MK"]
RESULTS["s2.res.step_both_from_full", RESULTS_DEV_MK_VS_MSAT] <- dev["RESULTS_DEV_MK_VS_MSAT"]


  # . Les modèles font tous mieux qu'un modèle constant ou un modèle saturés


## STRATEGIE 1 - Toutes Variables initiales ----

### Modèle complet 0 ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s1.res.glm.0

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

RESULTS["s1.res.glm.0", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s1.res.glm.0", RESULTS_PRECISION] <- precision
RESULTS["s1.res.glm.0", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcul la moyenne des distance entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s1.res.glm.0", RESULTS_ERREUR] <- erreur

### Modèle STEP forward ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s1.res.step_forward

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion
print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs
display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

RESULTS["s1.res.step_forward", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s1.res.step_forward", RESULTS_PRECISION] <- precision
RESULTS["s1.res.step_forward", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcul la moyenne des distance entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s1.res.step_forward", RESULTS_ERREUR] <- erreur

### Modèle STEP both from constant model ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s1.res.step_both_from_constant

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

RESULTS["s1.res.step_both_from_constant", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s1.res.step_both_from_constant", RESULTS_PRECISION] <- precision
RESULTS["s1.res.step_both_from_constant", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s1.res.step_both_from_constant", RESULTS_ERREUR] <- erreur


### Modèle STEP backward ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s1.res.step_backward

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s1.res.step_backward", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s1.res.step_backward", RESULTS_PRECISION] <- precision
RESULTS["s1.res.step_backward", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s1.res.step_backward", RESULTS_ERREUR] <- erreur


### Modèle STEP both from full model ----

#### Prédiction du jeu de validation ----
current.model.res.glm <- s1.res.step_both_from_full

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s1.res.step_both_from_full", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s1.res.step_both_from_full", RESULTS_PRECISION] <- precision
RESULTS["s1.res.step_both_from_full", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s1.res.step_both_from_full", RESULTS_ERREUR] <- erreur


## STRATEGIE 2 : Sélection de variables ----

### Modèle complet 0 ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s2.res.glm.0

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s2.res.glm.0", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s2.res.glm.0", RESULTS_PRECISION] <- precision
RESULTS["s2.res.glm.0", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s2.res.glm.0", RESULTS_ERREUR] <- erreur



### Modèle STEP forward ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s2.res.step_forward

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s2.res.step_forward", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s2.res.step_forward", RESULTS_PRECISION] <- precision
RESULTS["s2.res.step_forward", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s2.res.step_forward", RESULTS_ERREUR] <- erreur


### Modèle STEP both from constant model ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s2.res.step_both_from_constant

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s2.res.step_both_from_constant", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s2.res.step_both_from_constant", RESULTS_PRECISION] <- precision
RESULTS["s2.res.step_both_from_constant", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s2.res.step_both_from_constant", RESULTS_ERREUR] <- erreur


### Modèle STEP backward ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s2.res.step_backward

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s2.res.step_backward", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s2.res.step_backward", RESULTS_PRECISION] <- precision
RESULTS["s2.res.step_backward", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s2.res.step_backward", RESULTS_ERREUR] <- erreur


### Modèle STEP both from full model ----

#### Prédiction du jeu de validation ----

current.model.res.glm <- s2.res.step_both_from_full

# Prédiction des probabilités
current.model.res.predict <- predict(current.model.res.glm, newdata=dat.meteo.train[!scp.train,], type="response")

# Calcul du Seuil optimal selon critère "Taux de classification" (TN)
current.model.seuil_opt <- compute_opt_threshold (current.model.res.predict)
current.model.seuil_opt

# Décision selon seuil optimal
current.model.prediction <- (current.model.res.predict>current.model.seuil_opt)

#### Mesure de précision ----

# Matrice de confusion

# vérification 
length(scp.train[scp.train==FALSE])
length(dat.meteo.train$pluie.demain[!scp.train])
length(current.model.prediction)
length(current.model.res.predict)

print("Matrice de Confusion : ")
table(dat.meteo.train$pluie.demain[!scp.train], current.model.prediction)

# visualisation des faux positifs

display_TN_TP (current.model.res.predict, current.model.seuil_opt)

# Calcul de la précision TN+TP/N+P
precision <- compute_precision(current.model.prediction)
precision

# Courbe ROC
p <- display_ROC (current.model.res.predict)

# Calcul de l'AUC
auc <- compute_auc(p)
auc

# Store the results
RESULTS["s2.res.step_both_from_full", RESULTS_current.model.seuil_opt] <- current.model.seuil_opt
RESULTS["s2.res.step_both_from_full", RESULTS_PRECISION] <- precision
RESULTS["s2.res.step_both_from_full", RESULTS_AUC] <- auc

#### Mesure de l'erreur ----

# on calcule la moyenne des distances entre la valeur observée de pluie.demain et la probabilité prédite
erreur <- mean(abs(current.model.res.predict - dat.meteo.train$pluie.demain[!scp.train]))
erreur

RESULTS["s2.res.step_both_from_full", RESULTS_ERREUR] <- erreur



#__________________________________________________________________________
#
# CONCLUSION : Modèle choisi pour la prédiction ----
#
#__________________________________________________________________________


# visualisation des modèles
View(RESULTS)

# Meilleur modèle pour la prédiction du jeu de validation

# final.model.name <- "s2.res.step_both_from_constant"
# final.model.formula <- s2.res.step_both_from_constant$formula

final.model.name <- "s2.res.step_forward"
final.model.formula <- s2.res.step_forward$formula


#__________________________________________________________________________
#
# PREDICTION et DECISION ----
#
#__________________________________________________________________________

## Entrainement du meilleur modèle sur le jeu Train complet ----

final.model.res.glm <- glm(final.model.formula, data=dat.meteo.train, family="binomial")
summary(final.model.res.glm)

## Prédiction du jeu meteo.test ----

final.model.res.predict <- predict(final.model.res.glm, newdata=dat.meteo.test, type="response")
final.model.threshold <- RESULTS[final.model.name, RESULTS_current.model.seuil_opt]
final.model.threshold
final.model.prediction <- (final.model.res.predict >= final.model.threshold)

## Sauvegarde ----

## Ajout d'une colonne pluie.demain et Sauvegarde de la prédiction dans un fichier meteo.test.prediction.csv

dat.meteo.test.prediction <- cbind(dat.meteo.test.raw, pluie.demain=final.model.prediction)
write.csv(dat.meteo.test.prediction, "meteo.test.prediction.csv",
          quote=FALSE,
          row.names = FALSE)

# Contingence de **pluie.demain** prédit

table(dat.meteo.train[,"pluie.demain"])
table(dat.meteo.test.prediction[,"pluie.demain"])

# Vérification du chargement du fichier de prédiction et données associées 

dat.meteo.test.prediction <- read.table("meteo.test.prediction.csv", 
                                  sep=",", dec=".",
                                  header = 1,
                                  na.strings="NA",
                                  colClasses = c(rep('numeric', 46), 'logical'))

ncol(dat.meteo.test.prediction)
colnames(dat.meteo.test.prediction)
nrow(dat.meteo.test.prediction)

table(dat.meteo.test.prediction[,"pluie.demain"])



#__________________________________________________________________________
#
# SAUVEGARDE des DONNEES pour usage dans R Markdown ----
#
#__________________________________________________________________________

# Sauvegarde de l'image des données générées dans ce script
# Les données sont utilisées par le fichier projet_mlg.rmd
# présentant les résultats du projet
save.image(file="./projet_mlg.rdata")

