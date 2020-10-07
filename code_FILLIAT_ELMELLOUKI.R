####################################################################################################################################
###################### Chargement des jeux de données et premières transformations pour commencer à travailler #####################
####################################################################################################################################

rm(list = objects())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Data0 <- read.csv(file="train.csv")
Data1 <- read.csv(file="test.csv")

#Nous transformons les variables de dates afin qu'elles aient un format plus pratique à manipuler 
#et que nous puissions utiliser des opérateurs de comparaison
library(lubridate)
Data0$date <- strptime(as.character(Data0$date),format="%Y-%m-%d %H:%M:%S")
Data0$date <- as.POSIXct(Data0$date,tz = "UTC")

Data1$date <- strptime(as.character(Data1$date),format="%Y-%m-%d %H:%M:%S")
Data1$date <- as.POSIXct(Data1$date,tz = "UTC")


#On comble les valeurs manquantes
Data <-rbind(Data0[,-2], Data1[,-ncol(Data1)])
dim(Data)

#D'abord pour RH_6
Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
library(ranger)
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions

#Puis pour Visibility
Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)
Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions

#Nous réassignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]

####################################################################################################################################
###################### Analyse descriptive du jeu de données et examens préliminaires #####################
####################################################################################################################################

#Représentation de la consommation électrique totale du jeu de données
plot(Data0$date,Data0$Appliances,type='l',ylab='Consommation électrique',xlab='Date')

#Sur une semaine
plot(Data0$date[1:144*7],Data0$Appliances[1:144*7],type='l',ylab='Consommation électrique',xlab='Date')

#Sur une journée
plot(Data0$date[1:144],Data0$Appliances[1:144],type='l',ylab='Consommation électrique',xlab='Date')

# Représentations boîtes à moustache
col.pal<-colorRampPalette(c("lightblue", "red"))( 12 )
par(mfrow = c(2,1))
######### possible erreur (figure margins too large)
boxplot(Data0$Appliances~Data0$Heure,col=col.pal,outline=F,xlab = "Heure de la journée",ylab="Consommation en Wh")
boxplot(Data0$Appliances~Data0$Month,col=col.pal,outline=F,xlab = "Mois de l'année",ylab="Consommation en Wh")

####################################################################################################################################
###################### Analyse des matrices de correlation entre appliances et plusieurs autres variables
####################################################################################################################################

#variables de temperatures

#toutes les temperatures
temp<-cbind(Data0$Appliances, Data0$T1, Data0$T2, Data0$T3, Data0$T4, Data0$T5, Data0$T6, Data0$T7, Data0$T8, Data0$T9, Data0$T_out, Data0$Tdewpoint)
cov(temp)
cor(temp)
chart.Correlation(temp, histogram = TRUE) #prend quelques minutes a compiler

# une selection des temperatures les plus significatives et les moins redondantes, apparait dans le rapport
temp_reduit<-cbind(Data0$Appliances, Data0$T2, Data0$T3, Data0$T6)
cor(temp_reduit)
chart.Correlation(temp_reduit, histogram = TRUE) #qq minutes a compiler

# variables d'humidite relatives (idem)

RH<-cbind(Data0$Appliances, Data0$RH_1, Data0$RH_2, Data0$RH_3, Data0$RH_4, Data0$RH_5, Data0$RH_6, Data0$RH_7, Data0$RH_8, Data0$RH_9, Data0$RH_out)
cov(RH)
cor(RH)
chart.Correlation(RH, histogram = TRUE) # qq minutes a compiler

RH_reduit<-cbind(Data0$Appliances, Data0$RH_1, Data0$RH_8, Data0$RH_out)
cor(RH_reduit)
chart.Correlation(RH_reduit,histogram = TRUE) #apparait dans le rapport

# autres variables explicatives

Autres<-cbind(Data0$Appliances, Data0$NSM, Data0$lights, Data0$Windspeed)
cor(Autres)
chart.Correlation(Autres, histogram = TRUE) #apparait dans le rapport

####################################################################################################################################
###################### Analyse de la significativité des variables et première idée sur les variables pertinentes ##################
####################################################################################################################################

library(mlbench)
library(Boruta)

#On annule les valeurs de rv1 et rv2
Data0$rv1 = NULL
Data0$rv2 = NULL

#On convertit les facteurs et variables non numeriques en numeriques afin de pouvoir executer l'analyse en composantes principales ( qui ne supporte pas les facteurs)
Data0$WeekStatus = as.numeric(Data0$WeekStatus)
Data0$Day_of_week = as.numeric(Data0$Day_of_week)
Data0$DayType = as.numeric(Data0$DayType)
Data0$InstantF = as.numeric(Data0$InstantF)
Data0$WeekStatus = as.numeric(Data0$WeekStatus)
Data0$date = as.numeric(Data0$date)
Data0$rv1 = Data0$rv2 = NULL


library(FactoMineR)
PCA(Data0)

#Execution de l'algorithme d'analyse d'importance des variables
#Attention prend dans les 2h à compléter, je n'ai pas pu envoyer le .RData de la sortie
#car il est trop lourd
library(Boruta)
Boruta.Short <- Boruta(Appliances ~ ., data = Data0, doTrace = 2, ntree = 500)

#Représentation de l'importance des variables
plot(Boruta.Short)

#Code pour récupérer les variables classées par ordre croissant d'importance
Imp = Boruta.Short$ImpHistory
final_imp = Imp[99,]
names(final_imp[order(Imp[99,])])


####################################################################################################################################
###################### Modèles linéaire et non linéaire ############################################################################
####################################################################################################################################

######################## Premier Modèle : Modèle de regréssion linéaire
#Pour ce modèle nous procédons à un test stepwise de sélection de variables avec minimisation du critère AIC

#L'équation avec toutes les variables que va parcourir le modèle dans les deux sens
cov <- head(names(Data0)[-c(1,2)], 32)
eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
eq

#Création d'un jeu de données de valiadation croisée composé de 85% du jeu de données d'entraînement
training = Data0
#Pour la reproductibilité
set.seed(150)
#On crée la partition
library(caret)
Train_on <- createDataPartition(y = training$Appliances, p = 0.85, list = FALSE)  
training <- training[Train_on, ]
testing <- training[-Train_on, ]
#On conserve le jeu de donnée pour le test par la suite
testing_verif = testing
#On annule la colonne à prédire
testing$Appliances=NULL

#Le modèle complet à affiner 
full.model <- lm(eq, data = training)

#Appel de la fonction stepAIC qui va ajuster le modèle sur le jeu de données de validation croisée
library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=training)

#Prédiction sur le jeu de données de test
prediction_lm <- predict(step.model, newdata=testing)

#Représentation de la prédiction comparée aux valeurs réelles
par(mfrow=c(1,1))
plot(testing_verif$Appliances[1:144],type ='l',ylab="Appliances")
lines(prediction_lm[1:144],col='green',lwd='3')

library(Metrics)
rmse(prediction_lm,testing_verif$Appliances) #86.0234


######################## Second Modèle : Modèle de regréssion non linéaire
#Pour ce modèle nous avons choisi nous mêmes les variables et les regroupements en fonction de la pertinence que nous avons 
#exhibée lors de l'étude préliminaire

#Ajustement du modèle sur le jeu de données d'entraînement
#Nous avons choisi de garder le même afin de pouvoir comparer les résultats des différents modèles
library(mgcv)
g0 = gam(Appliances ~ InstantF + lights + s(T3,T9,T8,T2 , k=-1)+ Day_of_week + s(
            RH_3,RH_2,RH_1,RH_8,RH_5,RH_4,k=-1)+ T_out + Press_mm_hg + 
            Windspeed + Tdewpoint + Day_of_week+ InstantF +NSM, data = training)


#Prédiction
prediction_gam <- predict(g0, newdata=testing)

#On représente les deux modèles sur le même graphique pour voir graphiquement la différence
plot(testing_verif$Appliances[1:144],type ='l',ylab="Appliances")
lines(prediction_lm[1:144],col='red',lwd='3')
lines(prediction_gam[1:144],col='green',lwd='3')

#L'erreur de prédiction est plus faible, le modèle est donc meilleur, mais il peut être encore amélioré
rmse(prediction_gam,testing_verif$Appliances) #80.25694



####################################################################################################################################
###################### Modèle de régression par forêt aléatoire avec interpolation #################################################
####################################################################################################################################

##################### Première partie, complétion du jeu de données train par interpolation naïve
#Dans cette partie nous allons compléter par interpolation linéaire les valeurs manquantes du jeu de données d'entraînement 
#en utilisant les valeurs environnantes. Nous allons avant cela utiliser la fonction read_csv qui permet une visualisation plus propre des data frame

Data0 <- read_csv(file="train.csv")
Data1 <- read_csv(file="test.csv")

#On comble les valeurs manquantes
Data <-rbind(Data0[,-2], Data1[,-ncol(Data1)])
dim(Data)

#D'abord pour RH_6
Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
library(ranger)
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions

#Puis pour Visibility
Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)
Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions

#Nous réassignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]

#Les indices correspondants aux individus antérieurs à la semaine de prédiction pure
n_l<-which(Data1$date<=as.Date("2016-05-19 23:50:00"))

#On récupère les lignes
Data1[n_l,]
fill = Data1[n_l,]

#On crée une nouvelle colonne Appliance puis on enlève la colonne Id qui ne serta à rien
fill$Appliances = NA
fill$Id = NULL

#On crée une nouvelle data frame qui va contenir toutes les valeurs du début à la fin sans sauts de temps
Data = rbind(Data0,fill)
Data = arrange(Data, Data$date)
#On s'assure bien que les valeurs sont bonnes et que l'on a réussi à faire ce que l'on souhaitait
Data[1:15,]
#On remarque bien que l'on voit apparaître des lignes là où elles manquaient et que l'on a bien des NA là on l'Appliance manque

#On récupère maintenant les lignes contenant les valeurs à interpoler. C'est important de le stocker car il faudra par la suite remettre dans ces mêmes
#lignes les valeurs interpolées
k = which(is.na(Data$Appliances))
k
#On voit bien que l'on ne recupère que les lignes où l'Appliance manque
Data[k,]

#On utilise la fonction d'interpolation afin de faire ce que l'on souhaite
library(imputeTS)
Data$Appliances = na_interpolation(Data$Appliances)

#On s'assure que l'on a réussi à faire ce que l'on souhaitait
points(Data[1:50,]$date,Data[1:50,]$Appliances,col='blue',lwd='3')

#On stock la prédiciton dans le fichier à soumettre
submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit[n_l,]$Appliances = Data[k,]$Appliances

#On vérifie les longeurs pour s'assurer que tout est bon 
length(k) == length(n_l)

# On représente la première partie de la prédiction. Ce sont les valeurs que l'on a interpolées
plot(Data0$date, Data0$Appliances, type='l', xlab="Date",ylab="Appliance")
lines(Data1$date,submit$Appliances,col='red')



##################### Deuxième partie, complétion du jeu de données train par régression linéaire et forêt aléatoire
#Dans cette partie nous allons compléter par des modèles plus complexes les valeurs manquantes du jeu de données d'entraînement 
#en utilisant les valeurs environnantes mais également les variables explicatives. 
setwd("C:/Users/harold/Desktop/STA/SIM202")
Data0 <- read.csv(file="train.csv")
Data1 <- read.csv(file="test.csv")

Data0$date <- strptime(as.character(Data0$date),format="%Y-%m-%d %H:%M:%S")
Data0$date <- as.POSIXct(Data0$date,tz = "UTC")

Data1$date <- strptime(as.character(Data1$date),format="%Y-%m-%d %H:%M:%S")
Data1$date <- as.POSIXct(Data1$date,tz = "UTC")

#On comble les valeurs manquantes
Data <-rbind(Data0[,-2], Data1[,-ncol(Data1)])
dim(Data)

#D'abord pour RH_6
Data_WTHNA <- Data[-which(is.na(Data$RH_6)), ]
library(ranger)
RF_NA <- ranger(RH_6 ~ RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_7 + RH_8, data=Data_WTHNA)
Data$RH_6[which(is.na(Data$RH_6))] <- predict(RF_NA,data=Data[which(is.na(Data$RH_6)),])$predictions

#Puis pour Visibility
Data_WTHNA <- Data[-which(is.na(Data$Visibility)), ]
RF_NA <- ranger(Visibility ~ Tdewpoint + Windspeed + T_out + Instant, data=Data_WTHNA)
Data$Visibility[which(is.na(Data$Visibility))] <- predict(RF_NA, data=Data[which(is.na(Data$Visibility)),])$predictions

#Nous réassignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]


#################### Extrapolation à l'aide d'une foret aléatoire
#Nous commençons par récupérer les indices à prédire
n_l<-which(Data1$date<=as.Date("2016-05-19 23:50:00"))

#Nous construisons la data frame "à trous" que nous réorganisons avec la date
fill = Data1[n_l,]
fill$Appliances = NA
fill$Id = NULL
tail(fill)
Data = rbind(Data0,fill)
Data = arrange(Data, Data$date)
Data[1:15,]

#Les valeurs à compléter pour pouvoir les restocker 
k = which(is.na(Data$Appliances))
Data[k,]

#La fonction shift va nous permettre de décaler d'autant de pas de temps que l'on veut une colonnes
#On l'utilisera pour associer à un individu la valeur de l'appliance avant lui et après lui
shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )

#Un test pour vérifier que cela a marché
df = shift(Data0,1)
df


#On crée les colonnes de l'appliance shiftée
#L'appliance suivante
Appl_fwd = shift(Data0,-1)
Appl_fwd = Appl_fwd$Appliances

#L'appliance précédente
Appl_back = shift(Data0,1)
Appl_back = Appl_back$Appliances

#On vérifie encore une fois que cela correspond à ce que l'on souhaite
head(Data0)
Appl_back
Appl_fwd

#ON crée la data frame complétée contenant les appliances shiftée que l'on va utiliser afin d'ajuster un modèle 
#prenant en compte les valeurs passées et futures de l'Appliance
compl = Data0
compl$Appl_fwd = Appl_fwd
compl$Appl_back = Appl_back
#Une dernière vérification pour être sûr que cela correspond à ce qu'on veut
compl


#On met dans l'ordre les colonnes afin de bien voir les appliances futures passées et présentes côte à côte
#Cette data frame va être celle sur laquelle nous allons entraîner nos modèles 
compl[,c(1,2,44,45,3:43)]
compl = as.tibble(cbind(compl[,c(1,2,44,45,3:43)]))
compl

#On récupère les lignes à prédire
k = which(is.na(Data$Appliances))

# Nous allons faire le même travail afin de récupérer la data frame des individus à prévoir
Appl_fwd = shift(Data,-1)
Appl_fwd = Appl_fwd$Appliances
Appl_back = shift(Data,1)
Appl_back = Appl_back$Appliances

compl3 = Data
compl3$Appl_fwd = Appl_fwd
compl3$Appl_back = Appl_back
compl3

compl3[,c(1,2,44,45,3:43)]
compl3 = as.tibble(cbind(compl3[,c(1,2,44,45,3:43)]))
compl3
#Nous pouvons ici voir les valeurs à extrapoler en rouge : ce sont les NA

#On les récupère
k = which(is.na(compl3$Appliances))
to_predict = compl3[k,]

#On enlève la colonne Appliance
to_predict$Appliances = NULL
to_predict
#On a donc ce qu'il nous faut pour la prévision : pour chaque ligne, la valeur passée et la valeur future de l'Appliance
#Cependant on voit qu'il nous manque certaines valeurs dans Appl_fwd. Cela vient du fait que certaines valeurs manquantes se suivent. 
#Pour traiter cela pas le choix nous devons interpoler linéairement ces valeurs, car sinon nous ne pouvons rien faire
#L'erreur est cependant moindre et l'approximation correct car c'est toujours la moyenne de deux valeurs

#Nous interpolons
to_predict$Appl_fwd = na_interpolation(to_predict$Appl_fwd)
to_predict$Appl_back = na_interpolation(to_predict$Appl_back)

#On va maintenant ajuster un modèle permettant d'extrapoler de manière moins naïve les valeurs manquantes de l'Appliance

#D'abord à l'aide d'un random forest
rdf = ranger(formula = Appliances ~ Appl_fwd+Appl_back+lights+T1+RH_1+T2+RH_2+T3+RH_3+
               T4+RH_4+T5+RH_5+T6+RH_6+T7+
               RH_7+T8+RH_8+T9+RH_9+T_out+
               Press_mm_hg+RH_out+Windspeed+
               Tdewpoint+NSM+ WeekStatus+ Day_of_week,
             data = compl, num.trees = 5000)

prediction_rdf = predict(rdf,data = to_predict)

#Ensuite à l'aide d'un modèle linéaire
lm = lm(formula = Appliances ~ Appl_fwd+Appl_back+lights+T1+RH_1+T2+RH_2+T3+RH_3+
          T4+RH_4+T5+RH_5+T6+RH_6+T7+
          RH_7+T8+RH_8+T9+RH_9+T_out+
          Press_mm_hg+RH_out+Windspeed+
          Tdewpoint+NSM+ WeekStatus+ Day_of_week,
        data = compl)

prediction_lm = predict(lm,newdata = to_predict)

#On voit en les représentant que les deux extrapolation sont très proches
plot(prediction_lm,type='l')
lines(prediction_rdf$predictions,type="l",col='red')






############## Troisième partie : prédiction sur la semaine future à l'aide de notre modèle de random forest 

#Création d'un jeu de données de valiadation croisée composé de 85% du jeu de données d'entraînement
training = Data0
#Pour la reproductibilité
set.seed(150)
#On crée la partition
library(caret)
Train_on <- createDataPartition(y = training$Appliances, p = 0.85, list = FALSE)  
training <- training[Train_on, ]
testing <- training[-Train_on, ]
#On conserve le jeu de donnée pour le test par la suite
testing_verif = testing
#On annule la colonne à prédire
testing$Appliances=NULL

#Test du modèle par validation croisée
rdf2 = ranger(formula = Appliances ~ lights+T1+RH_1+T2+RH_2+T3+RH_3+
                T4+RH_4+T5+RH_5+T6+RH_6+T7+
                RH_7+T8+RH_8+T9+RH_9+T_out+
                Press_mm_hg+RH_out+Windspeed+
                Tdewpoint+NSM+ WeekStatus+ Day_of_week,
              data = training, num.trees = 5000)

#On teste sur un jeu de données de validation croisée
prediction = predict(rdf2, data = testing)
plot(testing_verif$date[1:500], testing_verif$Appliances[1:500],type='l',xlab = "Date",ylab = "Appliance")
lines(testing_verif$date[1:500],prediction$predictions[1:500],col='red')

library(Metrics)
rmse(prediction$predictions,testing_verif$Appliances)#29.974

#On va maintenant prédire sur la deuxième partie du modèle 
rdf2 = ranger(formula = Appliances ~ lights+T1+RH_1+T2+RH_2+T3+RH_3+
                T4+RH_4+T5+RH_5+T6+RH_6+T7+
                RH_7+T8+RH_8+T9+RH_9+T_out+
                Press_mm_hg+RH_out+Windspeed+
                Tdewpoint+NSM+ WeekStatus+ Day_of_week,
              data = Data0, num.trees = 5000)


#On récupère le futur à prévoir
n_l_2 <-which(Data1$date>as.Date("2016-05-19 23:50:00"))
futur = Data1[n_l_2,]

#On prédit dessus
prediction = predict(rdf2, data=futur)

#On le stock dans le fichier de soumission
submit[n_l_2,]$Appliances <- prediction$predictions

#On le représente
plot(Data1$date[n_l_2],submit[n_l_2,]$Appliances, col='red', type='l',xlab='Date',ylab="Prédiction de l'appliance sur la semaine suivante")

#On écrit finalement le fichier de soumission
write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)
