#==============================================================================#
#Ambiente R
options(prompt = "[RL] ", continue = "..  ", width = 70, useFancyQuotes = FALSE,
        #digits = 3, 
        warn = 1)

#==============================================================================#
#Librerias
library(plyr)
library(Hmisc)
library(psych) #function describre
library(ff)
library(RODBC)
library(gtools)
library(sas7bdat)

#==============================================================================#
#--------------------------- LUGAR DE TRABAJO ---------------------------------#
#==============================================================================#
ruta <- "D:/Configuraciones y Documentos/jherre14/Escritorio/Curso Introducción a R Trab/Regresión Lineal y Clustering"
memory.limit(size = 3999)
setwd(ruta)
getwd()
dir()

#source("Funciones_a_utilizar.r")


#==============================================================================#
#--------------------------- ARCHIVOS A OCUPAR --------------------------------#
#==============================================================================#

#------------------------------------------------------------------------------#
#Creamos conección

channel2 <- odbcConnectExcel2007(paste(ruta, "/Base Excel/","Base Datos FEM Final.xlsx", sep=""), readOnly = TRUE)



#------------------------------------------------------------------------------#
#Cargamos bases de datos


BaseFEM <- sqlFetch(channel2,"Hoja1",as.is=TRUE,
                    max=110000, rows_at_time=400)
names(BaseFEM)
dim(BaseFEM)
str(BaseFEM)


#==============================================================================#
#---------------------- Normalización de Variables-----------------------------#
#==============================================================================#

colnames(BaseFEM)[colnames(BaseFEM)=="dop_dia_mra"] <- paste("dia_mra", "_", 1, sep = "")


#==============================================================================#
#---------------------- Clustering k-means-----------------------------#
#==============================================================================#

# Load necessary libraries
library(datasets)

# Inspect data structure
str(BaseFEM)

# Subset the attitude data
dat = BaseFEM[,c(3,4)]

# Plot subset data
plot(dat, main = "jajaj", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)

# Plot results
plot(dat, col =(km1$cluster + 1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data

mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)



