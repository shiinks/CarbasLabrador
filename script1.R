library(dplyr)
library(tidyr)
library(ggfortify)
library(ggplot2)
library(ggcorrplot)
library(vegan)
library(mvpart)
library(MVPARTwrap)
library(rdaTest)
library(labdsv)
library(plyr)
library(ggrepel)

# raw data
raw<-read.csv("lakesUTF.csv", header = TRUE)


# select variables
env<-select(raw, Site, Lac, LATITUDE, LONGITUDE, Region, SecchiDepth, MaxDepth, Temp, Conductivity, DOmgL, pH, Alcalinity, AbundanceBact,ZooBiomass, ChlA, TP, TN, NH4,NO3, Iron, DOC, DIC)
# region comme facteur
env[,5]<-as.factor(env[,5])
# remove na from all lakes
# env<-drop_na(env)


# Select variables
# Drop Alcalinity, DIC. (Correlated to conductivity), NO3 and NH4, long/lat,region,lac
env <- select(env, Site, pH, SecchiDepth, MaxDepth, Temp, Conductivity, DOmgL,  ChlA, TP, TN, Iron, DOC)
# remove na
env<-drop_na(env)
#SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL, AbundanceBact,ZooBiomass,
#Select data for analysis
df <- select(env,SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL,  ChlA, TP, TN, Iron, DOC)



#rda
env<-select(raw,SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL, TP, TN, Iron, DOC,ZooBiomass,AbundanceBact,ChlA)
env<-drop_na(env)
y <- select(env,SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL, TP, TN, Iron, DOC)
x<-select(env,ZooBiomass,AbundanceBact,ChlA)
rda1=rda(x,y,scaling=TRUE)#RDA Analysis
plot(rda1)


# PCA
#select Schefferville
envlab<-filter(env, Region == "Schefferville")

# Select variables
# Drop Alcalinity, DIC. (Correlated to conductivity), NO3 and NH4, long/lat,region,lac
envlab <- select(envlab, Site, pH, SecchiDepth, MaxDepth, Temp, Conductivity, DOmgL,ChlA, TP, TN, Iron, DOC)
# remove na
envlab<-drop_na(envlab)
#SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL, AbundanceBact,ZooBiomass,
#Select data for analysis
df <- select(envlab,SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL,   ChlA, TP, TN, Iron, DOC)
# PCA
options(scipen=999)

pca<-princomp(df,cor=TRUE)
screeplot(pca)
pca$loadings
summary(pca)

princomp(data[,],cor=FALSE)
pca <- princomp(data[,],cor=FALSE)
pca$loadings
> 0.5



pca_res <- prcomp(df, scale = TRUE)
autoplot(pca_res,data= envlab,label=TRUE,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
# Correlations
corr <- round(cor(df), 1)
ggcorrplot(corr)


# Cote nord
envcn<-filter(env, Region == "Côte-Nord")
# Select variables
# Drop  Alcalinity, DIC. (Correlated to conductivity), zoo biomass, NO3 and NH4, long/lat,region,lac
envcn<-select(envcn, Site, SecchiDepth, MaxDepth, Temp,pH, Alcalinity,  Conductivity, DOmgL, AbundanceBact, ChlA, TP, TN, Iron, DOC)
# Drop na
envcn<-drop_na(envcn)

# Select data for analysis
df <- select(envcn, SecchiDepth, MaxDepth, Temp,pH,  Conductivity, DOmgL, AbundanceBact, ChlA, TP, TN, Iron, DOC)

# PCA
pca_res <- prcomp(df, scale. = TRUE)
autoplot(pca_res,data= envcn,label=TRUE,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
# Correlations
corr <- round(cor(df), 1)
ggcorrplot(corr)

# Côte Nord + Schefferville
envcb<-envlab<-filter(env, Region == "Schefferville"|Region == "Côte-Nord")
envcb<-select(envcb, Site, Region, SecchiDepth, MaxDepth, Temp,pH, Alcalinity,  Conductivity, DOmgL, AbundanceBact, ChlA, TP, TN, Iron, DOC)
envcb<-drop_na(envcb)

df <- select(envcb,SecchiDepth, MaxDepth, Temp,pH,  Conductivity, DOmgL, AbundanceBact, ChlA, TP, TN, Iron, DOC)
pca_res <- prcomp(df, scale. = TRUE)
autoplot(pca_res,data= envcb,colour= 'Region',label=TRUE,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


#----------Hypothèse 1-------------------

phyto <- select(raw, Iron, ChlA, TP, TN)
phyto <- drop_na(phyto)
mod <- with(phyto, glm(ChlA ~ Iron + TP + TN )) #lm ou glm donne même données
summary(mod)
#Iron p = 0.0108
#TP p = 3.34e-6
#TN p = 5.06e-13

plot1 <- plot(phyto$ChlA ~ phyto$Iron
              , xlab = 'Concentration de fer'
              , ylab = 'ChlA')
plot2 <- plot(phyto$ChlA ~ phyto$TP
              , xlab = 'TP'
              , ylab = 'ChlA')
plot3 <- plot(phyto$ChlA ~ phyto$TN
              , xlab = 'TN'
              , ylab = 'ChlA')
#Pensez à une méthode pour bien présenter ces résultats
#Pourquoi les concentrations de fer varient en x?

ChlA <- as.numeric(phyto$ChlA)
TP <- as.numeric(phyto$TP)
TN <- as.numeric(phyto$TN)
Iron <- as.numeric(phyto$Iron)

plot(fitted(mod), resid(mod)) #patron problématique
qqnorm(resid(mod))
qqline(resid(mod)) #pas super
#Va falloir modifier les données

#Transformation des données (log)
phyto$ChlA <- log(phyto$ChlA+1)
phyto$TP <- log(phyto$TP+1)
phyto$TN <- log(phyto$TN+1)
phyto$Iron <- log(phyto$Iron+1) #Si on fait la transformation pour TN et/ou Iron tu as un message d'erreur quand tu fais ton modèle après?
               
#Modèle avec données modifiées
mod.log <- with(phyto, lm(ChlA ~ Iron + TP + TN )) 
summary(mod.log)

#seuil 3 = trop grand
vif(mod.log)

plot(fitted(mod.log), resid(mod.log)) #beaucoup mieux
qqnorm(resid(mod.log))
qqline(resid(mod.log)) #pas super....

#Transformation des données (exp)
phyto$ChlA <- exp(phyto$ChlA)
#phyto$Iron <- exp(phyto$Iron)
#phyto$TP <- exp(phyto$TP)
#phyto$TN <- exp(phyto$TN)

#Modèle avec transformation exponentielle
mod.exp <- with(phyto2, lm(exp(ChlA) ~ Iron + TP + TN ))
summary(mod.exp)

par(mfrow=c(2,2))
plot(mod.exp) #Vraiment pas bon
plot(fitted(mod.exp), resid(mod.exp)) #beaucoup mieux
qqnorm(resid(mod.exp))
qqline(resid(mod.exp)) #pas super....

max(phyto$ChlA)
phyto2 <- phyto[phyto$ChlA < 11,]

str(phyto)
summary(phyto2)
#Transformation des données (sqrt)
phyto$CHlA <- sqrt(phyto$ChlA)

#Modèle avec transformation racine carrée
mod.sqrt <- with(phyto, lm(ChlA ~ Iron + TP + TN))
summary(mod.sqrt)

par(mfrow=c(2,2))
plot(fitted(mod.sqrt), resid(mod.sqrt)) 
qqnorm(resid(mod.sqrt))
qqline(resid(mod.sqrt))
plot(mod.sqrt) #Vraiment pas bon

#---------Hypothèse 2---------

zoo <- select(raw, ZooBiomass, Iron, pH, Temp, ChlA, DOC, TP, TN, MaxDepth)
zoo<- drop_na(zoo)

#Création de plusieurs modèles afin de choisir le meilleur
library(AICcmodavg)
mod.test1 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA + DOC + TP + TN))
mod.test2 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA + DOC + TP))
mod.test3 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA))
mod.test4 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp))
mod.test5 <- with(zoo, lm(ZooBiomass ~ Iron + pH))
mod.test6 <- with(zoo, lm(ZooBiomass ~ Iron))
mod.test7 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA + DOC + TN))
mod.test8 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA + TN ))
mod.test9 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + TN))
mod.test10 <- with(zoo, lm(ZooBiomass ~ Iron + pH + TN))
mod.test11 <- with(zoo, lm(ZooBiomass ~ Iron + TN))
mod.test12 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + ChlA + TP + TN))
mod.test13 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + TP + TN))
mod.test14 <- with(zoo, lm(ZooBiomass ~ Iron + pH + TP + TN))
mod.test15 <- with(zoo, lm(ZooBiomass ~ Iron + TP + TN))
mod.test16 <- with(zoo, lm(ZooBiomass ~ Iron + TP))
mod.test17 <- with(zoo, lm(ZooBiomass ~ Iron + pH + Temp + DOC + TP + TN))
mod.test18 <- with(zoo, lm(ZooBiomass ~ Iron + pH + DOC + TP + TN))
mod.test19 <- with(zoo, lm(ZooBiomass ~ Iron + DOC + TP + TN))
mod.test20 <- with(zoo, lm(ZooBiomass ~ Iron + DOC + TP))
mod.test21 <- with(zoo, lm(ZooBiomass ~ Iron + DOC))
mod.test22 <- with(zoo, lm(ZooBiomass ~ Iron + pH + ChlA + DOC + TP + TN))
mod.test23 <- with(zoo, lm(ZooBiomass ~ Iron + ChlA + DOC + TP + TN))
mod.test24 <- with(zoo, lm(ZooBiomass ~ Iron + ChlA + DOC + TP))
mod.test25 <- with(zoo, lm(ZooBiomass ~ Iron + ChlA + DOC))
mod.test26 <- with(zoo, lm(ZooBiomass ~ Iron + ChlA))
mod.test27 <- with(zoo, lm(ZooBiomass ~ Iron + Temp + ChlA + DOC + TP + TN))
mod.test28 <- with(zoo, lm(ZooBiomass ~ Iron + Temp + ChlA + DOC + TP))
mod.test29 <- with(zoo, lm(ZooBiomass ~ Iron + Temp + ChlA + DOC))
mod.test30 <- with(zoo, lm(ZooBiomass ~ Iron + Temp + ChlA))
mod.test31 <- with(zoo, lm(ZooBiomass ~ Iron + Temp))
mod.test32 <- with(zoo, lm(ZooBiomass ~ 1))
AICc(mod.test1) #1478.68
AICc(mod.test2) #1477.84
AICc(mod.test3) #1478.28
AICc(mod.test4) #1476.055
AICc(mod.test5) #1474.37
AICc(mod.test6) #1474.45
AICc(mod.test7) #1477.84
AICc(mod.test8) #1480.002
AICc(mod.test9) #1477.75
AICc(mod.test10) #1476.02
AICc(mod.test11) #1476.048
AICc(mod.test12) #1480.16
AICc(mod.test13) #1478.115
AICc(mod.test14) #1476.39
AICc(mod.test15) #1476.12
AICc(mod.test16) #1475.55
AICc(mod.test17) #1477.61
AICc(mod.test18) #1475.35
AICc(mod.test19) #1475.9
AICc(mod.test20) #1475.59
AICc(mod.test21) #1473.71
AICc(mod.test22) #1476.7
AICc(mod.test23) #1477.5
AICc(mod.test24) #1476.9
AICc(mod.test25) #1475.49
AICc(mod.test26) #1476.53
AICc(mod.test27) #1479.23
AICc(mod.test28) #1478.58
AICc(mod.test29) #1477.37
AICc(mod.test30) #1478.166
AICc(mod.test31) #1475.98
AICc(mod.test32) #1400.815
listemodele <- list(mod.test1, mod.test2, mod.test3)
aictab(listemodele)

zoo <- select(raw, ZooBiomass, Iron)
zoo<- drop_na(zoo)
mod2 <- with(zoo, lm(ZooBiomass ~ Iron))
summary(mod2)
#Iron p = 0.271

plot4 <- plot(zoo$ZooBiomass ~ zoo$Iron
              , xlab = 'Concentration de fer'
              , ylab = 'Biomasse du zooplancton')
#Retirer les points à l'extrême droite?

plot(fitted(mod2), resid(mod2)) #patron problématique
qqnorm(resid(mod2))
qqline(resid(mod2))
#Va falloir modifier les données

#Transformation des données (log)
zoo$ZooBiomass <- log(zoo$ZooBiomass)
zoo$Iron<- log(zoo$Iron+1)
#Modèle avec données modifiées
mod2.log <- with(zoo, glm(ZooBiomass ~ Iron))
summary(mod2.log)

plot(fitted(mod2), resid(mod2)) #Pire
qqnorm(resid(mod2))
qqline(resid(mod2)) #Pire
#Il va falloir considérer autre chose qu'une transformation log ici

