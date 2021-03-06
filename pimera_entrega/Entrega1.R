library(ggplot2)
library(GGally)
library(dplyr) #For filter usage
library(gridExtra) #For subplots with ggplot
library(hrbrthemes)
library(latex2exp)

# Todos los pacientes son de sexo femenino mayores de 21 a�os de
# herencia Pima. Para este primer estudio, lo que buscaremos es encontrar 
# relaciones entre otras de las variables contenidas en el dataset.

# Punto 1
setwd("g:\\LUCAS\\Facultad\\Aprendizaje Estadistico\\Entregas\\Primera\\")
data<-read.csv("diabetes2.csv",header=T,sep=",")
data$Outcome<-as.factor(data$Outcome)


# Punto 2
lin_reg<-lm(data$BMI~data$Glucose+data$Pregnancies+data$BloodPressure+data$SkinThickness+data$Insulin+data$DiabetesPedigreeFunction+data$Age)
# Es correcto?  
#Cu�l es el modelo propuesto? verificar los puntos de BMI con qqnorm
#Y los supuestos del modelo? 
#Supuesto 1: existe linealidad
#Supuesto 2: E(epsilon)=0
#Supuesto 3:Var(epsilon)=sigma al cuadrado
#Supuesto 4: Independencia y no correlaci�n con las x_i
#Supuesto 5: Normalidad


#Punto 3

ggpairs(data)


#Punto 4

#Al ver los outcomes podemos ver que existe una mayor correlaci�n entre los 
#valores de la SkinThickness y los de BMI, por lo que si tuvieramos que 
#perder informaci�n eligiendo s�lo una variable de las contenidas en el dataset
#ser�a la SkinThickness

#Punto 5

# El test de significaci�n de la regresi�n es el test F 
# Para ver el nivel de significaci�n del test hay que mirar el p-valor 
#(entre m�s chico mejor). Sospechar si da muy chico (2,1.10-16)
#