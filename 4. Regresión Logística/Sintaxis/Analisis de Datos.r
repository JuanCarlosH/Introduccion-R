#==============================================================================#
#---------------------------- LIBRERIAS ---------------------------------------#
#==============================================================================#
#install.packages(c("sqldf","plyr","nFactors","RODBC","reshape","foreign","memisc","car"))
options(prompt = "[] ", continue = "..  ", width = 70, useFancyQuotes = FALSE,
digits = 3, warn = 1)
library(sqldf)
library(plyr)
library(nFactors)
library(RODBC)
library(reshape2)
library(reshape)
library(foreign)
library(memisc)
library(car)
library(data.table)
library(psych)
library(aod)
library(ggplot2)

#==============================================================================#
#--------------------------- LUGAR DE TRABAJO ---------------------------------#
#==============================================================================#
ruta <- "E:/Curso Introducción a R Reload/4. Regresión Logística"
memory.limit(size = 3999)
setwd(ruta)
getwd()
dir()

#==============================================================================#
#--------------------------- ARCHIVOS A OCUPAR --------------------------------#
#==============================================================================#
#Base Levantamiento
channel1<-odbcConnectExcel2007(paste(ruta,"/","Bases a Utilizar/","Base Simulada.xlsx", sep=""), readOnly = TRUE)


#------------------------------------------------------------------------------#
#Cargamos base Levantamiento
Levantamiento<-sqlFetch(channel1,"Resultados",as.is=TRUE,
max=110000, rows_at_time=400)

close(channel1)

names(Levantamiento)
dim(Levantamiento)
head(Levantamiento)
str(Levantamiento)


#------------------------------------------------------------------------------#
#----------------------Creación variable macrozonas----------------------------#
#------------------------------------------------------------------------------#

Levantamiento$MACROZONA<-
ifelse(Levantamiento$Region<=4 | Levantamiento$Region == 15, 1,
      ifelse(Levantamiento$Region >= 5 & Levantamiento$Region <= 8, 2,
          ifelse((Levantamiento$Region >= 9 & Levantamiento$Region <= 12) | Levantamiento$Region == 14, 3,
             ifelse(Levantamiento$Region == 13, 4, 0
))))

Levantamiento$MACROZONA1<-factor(Levantamiento$MACROZONA,
levels=c("1", "2", "3", "4"),
labels=c("Norte", "Centro", "Sur", "Metropolitana")
)

#==============================================================================#
#----------------------- Selección de Variables -------------------------------#
#==============================================================================#
#Chi-cuadrado

library(MASS)       # load the MASS package 
tbl <- table(Levantamiento$NroVisitas, Levantamiento$Responde) 
tbl  

chisq.test(tbl) 

#Information Value

#==============================================================================#
#----------------------- ANALISIS ELIGIBILIDAD --------------------------------#
#==============================================================================#
#Trabajamos con base de datos elegibles

BasePre <- Levantamiento

BasePre$Sexo <- factor(BasePre$Sexo)
BasePre$NivelEscolaridad <- factor(BasePre$NivelEscolaridad)
BasePre$NroVisitas <- factor(BasePre$NroVisitas)


#==============================================================================#
#------------------ MODELO DE REGRESION LOGISTICA -----------------------------#
#==============================================================================#
#Modelos iniciales
#Modelo 0
modelo0 <-
c("Responde ~ Edad + Sexo + NivelEscolaridad + NroVisitas ")

#==============================================================================#
#------------------------------ ESTIMACIÓN-------------------------------------#
#==============================================================================#

#Resumen de los modelos
#Modelo Full
mylogit0 <- glm(modelo0, data = BasePre, family = binomial("logit"))
summary(mylogit0)

describe(mylogit0$fitted.values)

mylogit0$rank

mylogit0$fitted.values

## CIs using profiled log-likelihood
confint(mylogit0)

## CIs using standard errors
confint.default(mylogit0)

## odds ratios only
exp(coef(mylogit0))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit0), confint(mylogit0)))

#-----------------------------------------------------------------------------#
#Curva ROC

library(pROC)

BasePre$Prob <- mylogit0$fitted.values

g <- roc(Responde ~ Prob, data = BasePre)
plot(g) 


