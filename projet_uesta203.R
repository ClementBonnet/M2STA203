
# ========== Importations ========== #

bike <- read.csv("SeoulBikeData.csv")

library(epiDisplay)
library(epiR)


# ========== Changements de types des variables ========== #

bike$Date <- as.Date(bike$Date, format="%d/%m/%Y")
bike$Holiday <- as.factor(bike$Holiday)
bike$Functioning.Day <- as.factor(bike$Functioning.Day)
bike$Seasons <- as.factor(bike$Seasons)
bike$Hour <- as.factor(bike$Hour)
bike$Humidity... <- as.numeric(bike$Humidity...)
bike$Visibility..10m. <- as.numeric(bike$Visibility..10m.)
str(bike)

# ========== Description des variables  ========== #

tab1(bike$Holiday)
tab1(bike$Seasons)
tab1(bike$Functioning.Day)

summary(bike$Rented.Bike.Count)
summary(bike$Hour)
summary(bike$Temperature..C.)
summary(bike$Humidity...)
summary(bike$Wind.speed..m.s.)
summary(bike$Visibility..10m.)
epi.descriptives(bike$Visibility..10m.)
summary(bike$Dew.point.temperature..C.)
summary(bike$Solar.Radiation..MJ.m2.)
summary(bike$Rainfall.mm.)
summary(bike$Snowfall..cm.)
bike$Snowfall..cm.


bike$nbv <- as.numeric(bike$Rented.Bike.Count)
epi.descriptives(bike$nbv)


# ========== Cr?ation de nouvelles variables ========== #

# Variable dichotomique de pluie
bike$rain <- ifelse(bike$Rainfall.mm. == 0, 0, 1)
# Variable dichotomique de neige
bike$snow <- ifelse(bike$Snowfall..cm. == 0, 0, 1)


tab1(bike$rain)
tab1(bike$snow)


# ========== Analyse univariée avec modèle de Poisson et mod?le de Quasi-Poisson ========== #

# Temp?rature
uniTemp <- glm(Rented.Bike.Count ~ Temperature..C., family = poisson, data = bike)
summary(uniTemp)

# Humidité
uniHumi <- glm(Rented.Bike.Count ~ Humidity..., family = poisson, data = bike)
summary(uniHumi)

# Vitesse du vent
uniWind <- glm(Rented.Bike.Count ~ Wind.speed..m.s., family = poisson, data = bike)
summary(uniWind)

# Visibilité
uniVisi <- glm(Rented.Bike.Count ~ Visibility..10m., family = poisson, data = bike)
summary(uniVisi)

# Point de rosée
uniDew <- glm(Rented.Bike.Count ~ Dew.point.temperature..C., family = poisson, data = bike)
summary(uniDew)

# Radiation solaire
uniSolar <- glm(Rented.Bike.Count ~ Solar.Radiation..MJ.m2., family = poisson, data = bike)
summary(uniSolar)

# Pluie
uniRainfall <- glm(Rented.Bike.Count ~ Rainfall.mm., family = poisson, data = bike)
summary(uniRainfall)

# Pluie OUI/NON
uniRain <- glm(Rented.Bike.Count ~ rain, family = poisson, data = bike)
summary(uniRain)

# Neige
uniSnowfall <- glm(Rented.Bike.Count ~ Snowfall..cm., family = poisson, data = bike)
summary(uniSnowfall)

# Neige OUI/NON
uniSnow <- glm(Rented.Bike.Count ~ snow, family = poisson, data = bike)
summary(uniSnow)

# Vacances
uniHoliday <- glm(Rented.Bike.Count ~ Holiday, family = poisson, data = bike)
summary(uniHoliday)

# Jour fonctionnel (pas jour férié)
uniFunction <- glm(Rented.Bike.Count ~ Functioning.Day, family = poisson, data = bike)
summary(uniFunction)

# Saisons
uniSeasons <- glm(Rented.Bike.Count ~ Seasons, family = poisson, data = bike)
summary(uniSeasons)

# ========== Corrélation ========== #

Corr <- cor(sapply(bike,as.numeric), use = "pairwise.complete.obs", method = "spearman")
corrplot::corrplot(Corr, method = "square", type = "upper", tl.col = "black")



# ========== Dispersion ========== #

moyenne <- mean(bike$Rented.Bike.Count)
variance <- var(bike$Rented.Bike.Count)
variance_corr <- var(bike$Rented.Bike.Count) * (nrow(bike)-1)/nrow(bike)


# ========== Modèles de Poisson complets ========== #

# ___ Mod?le initial ___ #
poisson1 <- glm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Holiday + Functioning.Day + Seasons, family = poisson, data = bike)
summary(poisson1) 

# ___ Mod?le sans Dew point, Rainfall et Snowfall mais avec rain et snow ___ #
poisson2 <- glm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Solar.Radiation..MJ.m2. + rain + snow + Holiday + Functioning.Day + Seasons, family = poisson, data = bike)
summary(poisson2)

# ___ Mod?le sans Dew point ___ #
poisson3 <- glm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Holiday + Functioning.Day + Seasons, family = poisson, data = bike)
summary(poisson3)

#Mod?le sans dew, avec Rain(Oui/Non) mais snow en continue
poisson4 <- glm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Solar.Radiation..MJ.m2. + rain + Snowfall..cm. + Holiday + Functioning.Day + Seasons, family = poisson, data = bike)
summary(poisson4)

# ___ Mod?le initial mais rain et snow au lieu de Rainfall et Snowfall ___ #
poisson5 <- glm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + rain + snow + Holiday + Functioning.Day + Seasons, family = poisson, data = bike)
summary(poisson5)


# ========== Calcul pour savoir si faire Quasi-Poisson ========== #

# Le rapport doit ?tre proche de 1 pour pouvoir utiliser poisson

quasioupas <- function(modele){
  return(sum(((bike$Rented.Bike.Count - modele$fitted.values)^2) / modele$fitted.values))
}

quasioupas(poisson1)

# ========== Mod?les de Quasi-Poisson ========== #

quasi4 <- glm(Rented.Bike.Count ~ Hour + I(Temperature..C./10) + I(Humidity.../10) + Wind.speed..m.s. + Visibility..10m. + Solar.Radiation..MJ.m2. + rain + I(Snowfall..cm./5) + Holiday + Functioning.Day + Seasons, family = quasipoisson, data = bike)
summary(quasi4)

round(cbind(exp(coef(quasi4)),exp(confint.default(quasi4))),2)
drop1(quasi4, test="Chisq")


# ========== Mod?les Binomiaux n?gatifs ========== #

nb4 <- glm.nb(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + Solar.Radiation..MJ.m2. + rain + Snowfall..cm. + Holiday + Functioning.Day + Seasons, data = bike)
summary(nb4)


#Graphique:
xb <- predict(nb4)
g <- cut(xb, breaks=quantile(xb,seq(0,100,5)/100))
m <- tapply(bike$Rented.Bike.Count, g, mean)
v <- tapply(bike$Rented.Bike.Count, g, var)
plot(m, v, xlab="Mean", ylab="Variance")
#
pr <- residuals(quasi4,"pearson")
phi <- sum(pr^2)/df.residual(quasi4)
phi
#
x <- seq(1,3000,10)
lines(x, x*phi, lty="dashed")
lines(x, x*(1+x/nb4$theta))


diff_bike_qp4 <- mean(abs(bike$Rented.Bike.Count - quasi4$fitted.values))
diff_bike_nb4 <- mean(abs(bike$Rented.Bike.Count - nb4$fitted.values))


moyennes_heures <- rep(NA,24)
for (i in c(1:24)){
  bike_sub <- subset(bike, bike$Hour == i-1)
  moyennes_heures[i]<- mean(bike_sub$Rented.Bike.Count)
}
moyennes_heures

# Ajout des valeurs prédites des modèles Quasi-Poisson et Binomial à la droite de 'bike'.
superbike <- cbind(bike, quasi4$fitted.values, nb4$fitted.values)
# Suppression des colonnes inutiles pour les graphiques.
hyperbike <- cbind(bike$Seasons, superbike[,2:3], superbike[,17:18])
# Changement de noms des colonnes
colnames(hyperbike) <- c("Saison", "NBV_obs", "Heure", "NBV_Quasi", "NBV_NB")
# Moyenne des vélos observée selon les heures
mean_obs <- hyperbike %>% group_by(Heure) %>% summarise(Moyenne = mean(NBV_obs))
# Moyenne des vélos prédite par le modèle de Quasi-Poisson, selon les heures
mean_quasi <- hyperbike %>% group_by(Heure) %>% summarise(Moyenne_Quasi = mean(NBV_Quasi))
# Moyenne des vélos prédite par le modèle Binomial négatif, selon les heures
mean_nb <- hyperbike %>% group_by(Heure) %>% summarise(Moyenne_NB = mean(NBV_NB))
# Regroupement des 3 vecteurs de moyennes pour pouvoir les comparer
mean_hour <- cbind(mean_obs, mean_quasi[,2], mean_nb[,2])

# Moyennes du nombre de vélos observée, prédite par QP et prédite par BN, selon l'heure, puis la saison.
ultrabike <- hyperbike %>% group_by(Heure, Saison) %>% summarise(MoyNBV_obs = mean(NBV_obs), MoyNBV_Quasi = mean(NBV_Quasi), MoyNBV_NB = mean(NBV_NB))

#plot(x = mean_hour$Heure, y = mean_hour$Moyenne, xlab="Heure", ylab="Moyenne de vélos", lty=5)


# 
# bike_printemps <- subset(hyperbike, hyperbike$Saison == "Spring")
# mean_obs <- bike_printemps %>% group_by(Heure) %>% summarise(Moyenne = mean(NBV_obs))
# mean_quasi <- bike_printemps %>% group_by(Heure) %>% summarise(Moyenne_Quasi = mean(NBV_Quasi))
# mean_nb <- bike_printemps %>% group_by(Heure) %>% summarise(Moyenne_NB = mean(NBV_NB))
# mean_printemps <- cbind(mean_obs, mean_quasi[,2], mean_nb[,2])
# 
# bike_ete <- subset(hyperbike, hyperbike$Saison == "Summer")
# mean_obs <- bike_ete %>% group_by(Heure) %>% summarise(Moyenne = mean(NBV_obs))
# mean_quasi <- bike_ete %>% group_by(Heure) %>% summarise(Moyenne_Quasi = mean(NBV_Quasi))
# mean_nb <- bike_ete %>% group_by(Heure) %>% summarise(Moyenne_NB = mean(NBV_NB))
# mean_ete <- cbind(mean_obs, mean_quasi[,2], mean_nb[,2])
# 
# bike_automne <- subset(hyperbike, hyperbike$Saison == "Autumn")
# mean_obs <- bike_automne %>% group_by(Heure) %>% summarise(Moyenne = mean(NBV_obs))
# mean_quasi <- bike_automne %>% group_by(Heure) %>% summarise(Moyenne_Quasi = mean(NBV_Quasi))
# mean_nb <- bike_automne %>% group_by(Heure) %>% summarise(Moyenne_NB = mean(NBV_NB))
# mean_automne <- cbind(mean_obs, mean_quasi[,2], mean_nb[,2])
# 
# bike_hiver <- subset(hyperbike, hyperbike$Saison == "Winter")
# mean_obs <- bike_hiver %>% group_by(Heure) %>% summarise(Moyenne = mean(NBV_obs))
# mean_quasi <- bike_hiver %>% group_by(Heure) %>% summarise(Moyenne_Quasi = mean(NBV_Quasi))
# mean_nb <- bike_hiver %>% group_by(Heure) %>% summarise(Moyenne_NB = mean(NBV_NB))
# mean_hiver <- cbind(mean_obs, mean_quasi[,2], mean_nb[,2])

ggplot(data = ultrabike, aes(x = Heure, y = MoyNBV_Quasi, group=Saison, colour=Saison)) + geom_line() + geom_point() + labs(title = "", x = "Heure", y = "Moyenne nombre de vélos")
ggplot(data = ultrabike, aes(x = Heure, y = MoyNBV_NB, group=Saison, colour=Saison)) + geom_line() + geom_point() + labs(title = "", x = "Heure", y = "Moyenne nombre de vélos")


