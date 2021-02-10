library(dplyr)
library(tidyr)
library(ggfortify)
library(ggplot2)
library(ggcorrplot)

# raw data
raw<-read.csv("lakes.csv", header = TRUE)

# select variables
env<-select(raw, Site, Lac, LATITUDE, LONGITUDE, Region, SecchiDepth, MaxDepth, Temp, Conductivity, DOmgL, pH, Alcalinity, AbundanceBact,ZooBiomass, ChlA, TP, TN, NH4,NO3, Iron, DOC, DIC)
# region comme facteur
env[,5]<-as.factor(env[,5])
# remove na from all lakes
# env<-drop_na(env)


#select Schefferville
envlab<-filter(env, Region == "Schefferville")

# Select variables
# Drop Alcalinity, DIC. (Correlated to conductivity), NO3 and NH4, long/lat,region,lac
envlab <- select(envlab, Site, pH, SecchiDepth, MaxDepth, Temp, Conductivity, DOmgL, AbundanceBact,ZooBiomass, ChlA, TP, TN, Iron, DOC)
# remove na
envlab<-drop_na(envlab)

#Select data for analysis
df <- select(envlab, SecchiDepth,pH, MaxDepth, Temp, Conductivity, DOmgL, AbundanceBact,ZooBiomass, ChlA, TP, TN, Iron, DOC)
# PCA
pca_res <- prcomp(df, scale. = TRUE)
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
