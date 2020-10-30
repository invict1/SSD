#-------------------------------------------------------------------------------------------------------
#Parte 2

rm( list=ls() )
gc()

install.packages('rpart')
install.packages('caret')
library(rpart)
library(rpart.plot)
library(caret)


dataset      <- read.csv("C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\datasetssd.txt", header = TRUE, sep = "\t")  
summary(dataset)

attach(dataset)
#1-
qqnorm(Tarjeta1_mconsumototal)
qqline(Tarjeta1_mconsumototal, col="red")

#2-
dataset["cliente_edad"]
hist(cliente_edad, prob = TRUE, col = "green", labels = TRUE, ylim = c(0, 0.035))

#3-
hist(Tarjeta1_mconsumospesos, prob = TRUE, col = "green")
dataset$datosFiltradosTarjeta= ifelse(Tarjeta1_mconsumospesos < 5000, Tarjeta1_mconsumospesos, NA)
hist(dataset$datosFiltradosTarjeta, prob = TRUE, col = "green")

#4-
boxplot(cliente_edad ~ clase_ternaria,
        main = "Comparar Edad vs Clase",
        xlab = "Clase ternaria de Cliente",
        ylab = "Edad del Cliente",
        col = rainbow(5))

