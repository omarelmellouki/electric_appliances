####################################################################################################################################
###################### Chargement des jeux de donn�es et premi�res transformations pour commencer � travailler #####################
####################################################################################################################################

rm(list = objects())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Data0 <- read.csv(file="train.csv")
Data1 <- read.csv(file="test.csv")

#Nous transformons les variables de dates afin qu'elles aient un format plus pratique � manipuler 
#et que nous puissions utiliser des op�rateurs de comparaison
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

#Nous r�assignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]

####################################################################################################################################
###################### Analyse descriptive du jeu de donn�es et examens pr�liminaires #####################
####################################################################################################################################

#Repr�sentation de la consommation �lectrique totale du jeu de donn�es
plot(Data0$date,Data0$Appliances,type='l',ylab='Consommation �lectrique',xlab='Date')

#Sur une semaine
plot(Data0$date[1:144*7],Data0$Appliances[1:144*7],type='l',ylab='Consommation �lectrique',xlab='Date')

#Sur une journ�e
plot(Data0$date[1:144],Data0$Appliances[1:144],type='l',ylab='Consommation �lectrique',xlab='Date')

# Repr�sentations bo�tes � moustache
col.pal<-colorRampPalette(c("lightblue", "red"))( 12 )
par(mfrow = c(2,1))
######### possible erreur (figure margins too large)
boxplot(Data0$Appliances~Data0$Heure,col=col.pal,outline=F,xlab = "Heure de la journ�e",ylab="Consommation en Wh")
boxplot(Data0$Appliances~Data0$Month,col=col.pal,outline=F,xlab = "Mois de l'ann�e",ylab="Consommation en Wh")

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
###################### Analyse de la significativit� des variables et premi�re id�e sur les variables pertinentes ##################
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
#Attention prend dans les 2h � compl�ter, je n'ai pas pu envoyer le .RData de la sortie
#car il est trop lourd
library(Boruta)
Boruta.Short <- Boruta(Appliances ~ ., data = Data0, doTrace = 2, ntree = 500)

#Repr�sentation de l'importance des variables
plot(Boruta.Short)

#Code pour r�cup�rer les variables class�es par ordre croissant d'importance
Imp = Boruta.Short$ImpHistory
final_imp = Imp[99,]
names(final_imp[order(Imp[99,])])


####################################################################################################################################
###################### Mod�les lin�aire et non lin�aire ############################################################################
####################################################################################################################################

######################## Premier Mod�le : Mod�le de regr�ssion lin�aire
#Pour ce mod�le nous proc�dons � un test stepwise de s�lection de variables avec minimisation du crit�re AIC

#L'�quation avec toutes les variables que va parcourir le mod�le dans les deux sens
cov <- head(names(Data0)[-c(1,2)], 32)
eq <- paste0("Appliances ~", paste0(cov, collapse='+'))
eq

#Cr�ation d'un jeu de donn�es de valiadation crois�e compos� de 85% du jeu de donn�es d'entra�nement
training = Data0
#Pour la reproductibilit�
set.seed(150)
#On cr�e la partition
library(caret)
Train_on <- createDataPartition(y = training$Appliances, p = 0.85, list = FALSE)  
training <- training[Train_on, ]
testing <- training[-Train_on, ]
#On conserve le jeu de donn�e pour le test par la suite
testing_verif = testing
#On annule la colonne � pr�dire
testing$Appliances=NULL

#Le mod�le complet � affiner 
full.model <- lm(eq, data = training)

#Appel de la fonction stepAIC qui va ajuster le mod�le sur le jeu de donn�es de validation crois�e
library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = TRUE, 
                      data=training)

#Pr�diction sur le jeu de donn�es de test
prediction_lm <- predict(step.model, newdata=testing)

#Repr�sentation de la pr�diction compar�e aux valeurs r�elles
par(mfrow=c(1,1))
plot(testing_verif$Appliances[1:144],type ='l',ylab="Appliances")
lines(prediction_lm[1:144],col='green',lwd='3')

library(Metrics)
rmse(prediction_lm,testing_verif$Appliances) #86.0234


######################## Second Mod�le : Mod�le de regr�ssion non lin�aire
#Pour ce mod�le nous avons choisi nous m�mes les variables et les regroupements en fonction de la pertinence que nous avons 
#exhib�e lors de l'�tude pr�liminaire

#Ajustement du mod�le sur le jeu de donn�es d'entra�nement
#Nous avons choisi de garder le m�me afin de pouvoir comparer les r�sultats des diff�rents mod�les
library(mgcv)
g0 = gam(Appliances ~ InstantF + lights + s(T3,T9,T8,T2 , k=-1)+ Day_of_week + s(
            RH_3,RH_2,RH_1,RH_8,RH_5,RH_4,k=-1)+ T_out + Press_mm_hg + 
            Windspeed + Tdewpoint + Day_of_week+ InstantF +NSM, data = training)


#Pr�diction
prediction_gam <- predict(g0, newdata=testing)

#On repr�sente les deux mod�les sur le m�me graphique pour voir graphiquement la diff�rence
plot(testing_verif$Appliances[1:144],type ='l',ylab="Appliances")
lines(prediction_lm[1:144],col='red',lwd='3')
lines(prediction_gam[1:144],col='green',lwd='3')

#L'erreur de pr�diction est plus faible, le mod�le est donc meilleur, mais il peut �tre encore am�lior�
rmse(prediction_gam,testing_verif$Appliances) #80.25694



####################################################################################################################################
###################### Mod�le de r�gression par for�t al�atoire avec interpolation #################################################
####################################################################################################################################

##################### Premi�re partie, compl�tion du jeu de donn�es train par interpolation na�ve
#Dans cette partie nous allons compl�ter par interpolation lin�aire les valeurs manquantes du jeu de donn�es d'entra�nement 
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

#Nous r�assignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]

#Les indices correspondants aux individus ant�rieurs � la semaine de pr�diction pure
n_l<-which(Data1$date<=as.Date("2016-05-19 23:50:00"))

#On r�cup�re les lignes
Data1[n_l,]
fill = Data1[n_l,]

#On cr�e une nouvelle colonne Appliance puis on enl�ve la colonne Id qui ne serta � rien
fill$Appliances = NA
fill$Id = NULL

#On cr�e une nouvelle data frame qui va contenir toutes les valeurs du d�but � la fin sans sauts de temps
Data = rbind(Data0,fill)
Data = arrange(Data, Data$date)
#On s'assure bien que les valeurs sont bonnes et que l'on a r�ussi � faire ce que l'on souhaitait
Data[1:15,]
#On remarque bien que l'on voit appara�tre des lignes l� o� elles manquaient et que l'on a bien des NA l� on l'Appliance manque

#On r�cup�re maintenant les lignes contenant les valeurs � interpoler. C'est important de le stocker car il faudra par la suite remettre dans ces m�mes
#lignes les valeurs interpol�es
k = which(is.na(Data$Appliances))
k
#On voit bien que l'on ne recup�re que les lignes o� l'Appliance manque
Data[k,]

#On utilise la fonction d'interpolation afin de faire ce que l'on souhaite
library(imputeTS)
Data$Appliances = na_interpolation(Data$Appliances)

#On s'assure que l'on a r�ussi � faire ce que l'on souhaitait
points(Data[1:50,]$date,Data[1:50,]$Appliances,col='blue',lwd='3')

#On stock la pr�diciton dans le fichier � soumettre
submit <- read.csv(file="sample_submission.csv", sep=",", dec=".")
submit[n_l,]$Appliances = Data[k,]$Appliances

#On v�rifie les longeurs pour s'assurer que tout est bon 
length(k) == length(n_l)

# On repr�sente la premi�re partie de la pr�diction. Ce sont les valeurs que l'on a interpol�es
plot(Data0$date, Data0$Appliances, type='l', xlab="Date",ylab="Appliance")
lines(Data1$date,submit$Appliances,col='red')



##################### Deuxi�me partie, compl�tion du jeu de donn�es train par r�gression lin�aire et for�t al�atoire
#Dans cette partie nous allons compl�ter par des mod�les plus complexes les valeurs manquantes du jeu de donn�es d'entra�nement 
#en utilisant les valeurs environnantes mais �galement les variables explicatives. 
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

#Nous r�assignons les valeurs dans les colonnes correspondantes
Data0[,-2] <- Data[1:nrow(Data0),]
Data1[,-ncol(Data1)] <- Data[(nrow(Data0)+1):nrow(Data),]


#################### Extrapolation � l'aide d'une foret al�atoire
#Nous commen�ons par r�cup�rer les indices � pr�dire
n_l<-which(Data1$date<=as.Date("2016-05-19 23:50:00"))

#Nous construisons la data frame "� trous" que nous r�organisons avec la date
fill = Data1[n_l,]
fill$Appliances = NA
fill$Id = NULL
tail(fill)
Data = rbind(Data0,fill)
Data = arrange(Data, Data$date)
Data[1:15,]

#Les valeurs � compl�ter pour pouvoir les restocker 
k = which(is.na(Data$Appliances))
Data[k,]

#La fonction shift va nous permettre de d�caler d'autant de pas de temps que l'on veut une colonnes
#On l'utilisera pour associer � un individu la valeur de l'appliance avant lui et apr�s lui
shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )

#Un test pour v�rifier que cela a march�
df = shift(Data0,1)
df


#On cr�e les colonnes de l'appliance shift�e
#L'appliance suivante
Appl_fwd = shift(Data0,-1)
Appl_fwd = Appl_fwd$Appliances

#L'appliance pr�c�dente
Appl_back = shift(Data0,1)
Appl_back = Appl_back$Appliances

#On v�rifie encore une fois que cela correspond � ce que l'on souhaite
head(Data0)
Appl_back
Appl_fwd

#ON cr�e la data frame compl�t�e contenant les appliances shift�e que l'on va utiliser afin d'ajuster un mod�le 
#prenant en compte les valeurs pass�es et futures de l'Appliance
compl = Data0
compl$Appl_fwd = Appl_fwd
compl$Appl_back = Appl_back
#Une derni�re v�rification pour �tre s�r que cela correspond � ce qu'on veut
compl


#On met dans l'ordre les colonnes afin de bien voir les appliances futures pass�es et pr�sentes c�te � c�te
#Cette data frame va �tre celle sur laquelle nous allons entra�ner nos mod�les 
compl[,c(1,2,44,45,3:43)]
compl = as.tibble(cbind(compl[,c(1,2,44,45,3:43)]))
compl

#On r�cup�re les lignes � pr�dire
k = which(is.na(Data$Appliances))

# Nous allons faire le m�me travail afin de r�cup�rer la data frame des individus � pr�voir
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
#Nous pouvons ici voir les valeurs � extrapoler en rouge : ce sont les NA

#On les r�cup�re
k = which(is.na(compl3$Appliances))
to_predict = compl3[k,]

#On enl�ve la colonne Appliance
to_predict$Appliances = NULL
to_predict
#On a donc ce qu'il nous faut pour la pr�vision : pour chaque ligne, la valeur pass�e et la valeur future de l'Appliance
#Cependant on voit qu'il nous manque certaines valeurs dans Appl_fwd. Cela vient du fait que certaines valeurs manquantes se suivent. 
#Pour traiter cela pas le choix nous devons interpoler lin�airement ces valeurs, car sinon nous ne pouvons rien faire
#L'erreur est cependant moindre et l'approximation correct car c'est toujours la moyenne de deux valeurs

#Nous interpolons
to_predict$Appl_fwd = na_interpolation(to_predict$Appl_fwd)
to_predict$Appl_back = na_interpolation(to_predict$Appl_back)

#On va maintenant ajuster un mod�le permettant d'extrapoler de mani�re moins na�ve les valeurs manquantes de l'Appliance

#D'abord � l'aide d'un random forest
rdf = ranger(formula = Appliances ~ Appl_fwd+Appl_back+lights+T1+RH_1+T2+RH_2+T3+RH_3+
               T4+RH_4+T5+RH_5+T6+RH_6+T7+
               RH_7+T8+RH_8+T9+RH_9+T_out+
               Press_mm_hg+RH_out+Windspeed+
               Tdewpoint+NSM+ WeekStatus+ Day_of_week,
             data = compl, num.trees = 5000)

prediction_rdf = predict(rdf,data = to_predict)

#Ensuite � l'aide d'un mod�le lin�aire
lm = lm(formula = Appliances ~ Appl_fwd+Appl_back+lights+T1+RH_1+T2+RH_2+T3+RH_3+
          T4+RH_4+T5+RH_5+T6+RH_6+T7+
          RH_7+T8+RH_8+T9+RH_9+T_out+
          Press_mm_hg+RH_out+Windspeed+
          Tdewpoint+NSM+ WeekStatus+ Day_of_week,
        data = compl)

prediction_lm = predict(lm,newdata = to_predict)

#On voit en les repr�sentant que les deux extrapolation sont tr�s proches
plot(prediction_lm,type='l')
lines(prediction_rdf$predictions,type="l",col='red')






############## Troisi�me partie : pr�diction sur la semaine future � l'aide de notre mod�le de random forest 

#Cr�ation d'un jeu de donn�es de valiadation crois�e compos� de 85% du jeu de donn�es d'entra�nement
training = Data0
#Pour la reproductibilit�
set.seed(150)
#On cr�e la partition
library(caret)
Train_on <- createDataPartition(y = training$Appliances, p = 0.85, list = FALSE)  
training <- training[Train_on, ]
testing <- training[-Train_on, ]
#On conserve le jeu de donn�e pour le test par la suite
testing_verif = testing
#On annule la colonne � pr�dire
testing$Appliances=NULL

#Test du mod�le par validation crois�e
rdf2 = ranger(formula = Appliances ~ lights+T1+RH_1+T2+RH_2+T3+RH_3+
                T4+RH_4+T5+RH_5+T6+RH_6+T7+
                RH_7+T8+RH_8+T9+RH_9+T_out+
                Press_mm_hg+RH_out+Windspeed+
                Tdewpoint+NSM+ WeekStatus+ Day_of_week,
              data = training, num.trees = 5000)

#On teste sur un jeu de donn�es de validation crois�e
prediction = predict(rdf2, data = testing)
plot(testing_verif$date[1:500], testing_verif$Appliances[1:500],type='l',xlab = "Date",ylab = "Appliance")
lines(testing_verif$date[1:500],prediction$predictions[1:500],col='red')

library(Metrics)
rmse(prediction$predictions,testing_verif$Appliances)#29.974

#On va maintenant pr�dire sur la deuxi�me partie du mod�le 
rdf2 = ranger(formula = Appliances ~ lights+T1+RH_1+T2+RH_2+T3+RH_3+
                T4+RH_4+T5+RH_5+T6+RH_6+T7+
                RH_7+T8+RH_8+T9+RH_9+T_out+
                Press_mm_hg+RH_out+Windspeed+
                Tdewpoint+NSM+ WeekStatus+ Day_of_week,
              data = Data0, num.trees = 5000)


#On r�cup�re le futur � pr�voir
n_l_2 <-which(Data1$date>as.Date("2016-05-19 23:50:00"))
futur = Data1[n_l_2,]

#On pr�dit dessus
prediction = predict(rdf2, data=futur)

#On le stock dans le fichier de soumission
submit[n_l_2,]$Appliances <- prediction$predictions

#On le repr�sente
plot(Data1$date[n_l_2],submit[n_l_2,]$Appliances, col='red', type='l',xlab='Date',ylab="Pr�diction de l'appliance sur la semaine suivante")

#On �crit finalement le fichier de soumission
write.table(submit, file="submission_lm.csv", quote=F, sep=",", dec='.',row.names = F)
