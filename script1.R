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

#---------Hypothèse 2---------

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

