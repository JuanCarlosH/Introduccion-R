#==============================================================================#
#------------------------ Introducción y Preliminares -------------------------#
#==============================================================================#


#==============================================================================#
#--------------Manipulaciones Sencillas: Numeros y Vectores--------------------#
#==============================================================================#

#Asignación de vectores

a <- c(1, 2, 5, 3, 6, -2, 4)
a

b <- c("one", "two", "three")
b

c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
c


#==============================================================================#
#---------------------------- Vectores y Matrices -----------------------------#
#==============================================================================#

#Creando Vectores

a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]

a[c(1, 3, 5)]

a[2:6]

#Matrices

y <- matrix(1:20, nrow=5, ncol=4) 
y

cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
mymatrix

mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE,
                   dimnames=list(rnames, cnames))
mymatrix


#Using matrix subscripts
x <- matrix(1:10, nrow=2)
x

x[2,]

x[,2]

x[1,4]

x[1, c(4,5)]

#------------------------------------------------------------------------------#
#Operaciones Matriciales
# suma
# Resta
# Multiplicación
# Transpuestas
# Inversas


#==============================================================================#
#------------------------------- Marcos de datos ------------------------------#
#==============================================================================#

#Trabajos con marcos de datos o data.frame
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

#Resumenes 
summary(patientdata$age)

#Specifying elements of a data frame
patientdata[1:2]


patientdata$age

#Tablas de Frecuencias
table(patientdata$diabetes)

table(patientdata$diabetes, patientdata$status)


#cbind y rbind

#attach y detach

attach(patientdata)
summary(age)

detach(patientdata)
summary(age)

#Using factors
patientID <- c(1, 2, 3, 4) 
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)


#==============================================================================#
#--------------------------- Manipulación de datos ----------------------------#
#==============================================================================#

#selección de variables
vbs <- c("diabetes", "status")
patientdata1 <- patientdata[vbs]

#subconjunto de observaciones y variables
patientdata2 <- subset(patientdata, age >= 33, select = c("patientID", "status"))

#Agrupación
library(plyr)

Res <- ddply(patientdata, c("diabetes"), summarise,
             n = length(patientID),
             media = mean(age),
             Desv = sd(age),
             Min = min(age),
             max = max(age))



#==============================================================================#
#------------------------------ Lectura de Datos ------------------------------#
#==============================================================================#



#==============================================================================#
#----------------------- Distribuciones de Probabilidad------------------------#
#==============================================================================#

#Generar numeros aleatoria
#Tets de Normalidad
# Prueba para 


#==============================================================================#
#-------------------------- Escribir funciones en R----------------------------#
#==============================================================================#


#==============================================================================#
#-------------------------- Análisis Estadístico en R--------------------------#
#==============================================================================#
#Estadísticas Descriptivas
#Cluster k-medias
#Modelo de Regresión Lineal
#Modelos de Regresión Logística



#==============================================================================#
#-------------------------- Procedimientos Graficos----------------------------#
#==============================================================================#
#Histogramas + densidad

#Boxplots

#Dispersión

#Barras

#Tortas

#Piramidales

#Matrices de Graficos

#Graficos 3d


#Comparing Drug A and Drug B response by dose
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
#windows()
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b",
     pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",
      pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5)
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
par(opar)





































































#------------------------------------------------------------------------------#
#Una muestra de R
#------------------------------------------------------------------------------#

age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)

sd(weight)

cor(age,weight)

plot(age,weight)
q()

#An example of commands used to manage the R workspace

setwd("C:/myprojects/project1")
options()
options(digits=3)
x <- runif(20)
summary(x)
hist(x)
savehistory()
save.image()
q()

#Working with a new package
help.start()
install.packages("vcd")
help(package="vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
q()




