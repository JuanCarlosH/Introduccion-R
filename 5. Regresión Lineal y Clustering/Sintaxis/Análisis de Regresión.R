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
# Cargamos función para decilizar 

deciles <- function(x)
{
  deciles <- vector(length = 10)
  for (i in seq(0.1,1,.1))
    {
    deciles[i*10] <- quantile(x, i)
    }
  return (ifelse(x<deciles[1], 1,
          ifelse(x<deciles[2], 2,
          ifelse(x<deciles[3], 3,
          ifelse(x<deciles[4], 4,
          ifelse(x<deciles[5], 5,
          ifelse(x<deciles[6], 6,
          ifelse(x<deciles[7], 7,
          ifelse(x<deciles[8], 8,
          ifelse(x<deciles[9], 9, 10)
          )))))))))
}



#------------------------------------------------------------------------------#
#Cargamos bases de datos


BaseFEM <- sqlFetch(channel2,"Hoja1",as.is=TRUE,
                   max=110000, rows_at_time=400)
names(BaseFEM)
dim(BaseFEM)
str(BaseFEM)


#==============================================================================#
#------------------------- Estadística Descriptiva-----------------------------#
#==============================================================================#

#------------------------------------------------------------------------------#
# Estadísticas Descriptivas

names(BaseFEM)

describe(BaseFEM)

#Eliminar variable F12*


#------------------------------------------------------------------------------#
#histogramas

windows()
hist(BaseFEM$edad, 
     xlab = "Edad en años", 
     ylab = "", main = "", 
     #ylim = c(0, 0.007),
     probability = TRUE, 
    col = "lightblue")
lines(density(Tabla_1$Ptje_SICA), col = "red")


windows()
hist(BaseFEM$talla, 
     xlab = "Talla en Cms", 
     ylab = "", main = "", 
     #ylim = c(0, 0.008),
     probability = TRUE, 
     col = "lightblue")
lines(density(Tabla_1$Ptje_SICA), col = "red")

windows()
hist(BaseFEM$peso, 
     xlab = "Peso en Kgs", 
     ylab = "", main = "", 
     #ylim = c(0, 0.008),
     probability = TRUE, 
     col = "lightblue")
lines(density(Tabla_1$Ptje_SICA), col = "red")

windows()
hist(BaseFEM$pefmejor, 
     xlab = "PEF Mejor", 
     ylab = "", main = "", 
     #ylim = c(0, 0.008),
     probability = TRUE, 
     col = "lightblue")
lines(density(Tabla_1$Ptje_SICA), col = "red")

#------------------------------------------------------------------------------#
#Calculamos deciles



#------------------------------------------------------------------------------#
# Multiple Linear Regression Example 
fit <- lm(pefmejor ~ edad + talla + peso, data = BaseFEM)
summary(fit) # show results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# compare models
fit1 <- lm(pefmejor ~ edad + talla + peso, data = BaseFEM)
fit2 <- lm(pefmejor ~ talla + peso, data = BaseFEM)
anova(fit1, fit2)



