#Arbol con libreria  rpart
#se calculan las ganancias con CINCO semillas distintas  


#limpio la memoria
rm( list=ls() )
gc()

#si es necesario, instalar las librerias previamente, ej install.packages('rpart')
library(rpart)
library(rpart.plot)
library(caret)


archivo_entrada      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\datasetssd.txt"
campos_separador     <-  "\t"
campo_id             <-  "numero_de_cliente"
clase_nomcampo       <-  "clase_ternaria"
clase_valor_positivo <-  "BAJA+2"
campos_a_borrar      <-  c( )

#crear un directorio, ej C:\SalidaModelo2
archivo_imagen1      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\arbol_2_1.jpg"
archivo_imagen2      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\arbol_2_2.jpg"
archivo_imagen3      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\arbol_2_3.jpg"
archivo_imagen4      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\arbol_2_4.jpg"
archivo_imagen5      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\arbol_2_5.jpg"

archivo_summary1     <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\summary_2_1.txt"
archivo_summary2     <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\summary_2_2.txt"
archivo_summary3     <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\summary_2_3.txt"
archivo_summary4     <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\summary_2_4.txt"
archivo_summary5     <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo2\\summary_2_5.txt"

prob_training        <-  0.7
prob_testing         <-  1 - prob_training



#------------------------------------------------------
#definicion  funcion ganancia para nuestro problema

fganancia = function( probs, clases )
{
 
  return(  sum(    (probs >( 250/10000) ) * 
                   ifelse( clases== clase_valor_positivo, 9750, -250 )   
              )
         )

}
#------------------------------------------------------



#cargo los datos
dataset <- read.table( archivo_entrada, header=TRUE, sep=campos_separador, row.names=campo_id )


#borro las variables que no me interesan
dataset <- dataset[ , !(names(dataset) %in%   campos_a_borrar  )    ] 


#------------------------------------------------------
#uno

#genero  datasets de training y testing
set.seed( 100069 )

inTraining        <-  createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]



# generacion del modelo
formula  <-  formula( paste(clase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset_training,   cp=0.005,  xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



#dibujo el arbol en un archivo
jpeg(file = archivo_imagen1,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()

#grabo el summary del modelo
summary( modelo, file= archivo_summary1 )

#aplico el modelo a datos nuevos
testing_prediccion  <- predict(  modelo, dataset_testing , type = "prob")

# calculo la ganancia normalizada  en testing
ganancia_normalizada1 <- fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[ , clase_nomcampo] ) / prob_testing

#---------------------------------
#dos

#genero  datasets de training y testing
set.seed( 100193 )

inTraining        <-  createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]



# generacion del modelo
formula  <-  formula( paste(clase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset_training,   cp=0.005,  xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



#dibujo el arbol en un archivo
jpeg(file = archivo_imagen2,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()

#grabo el summary del modelo
summary( modelo, file= archivo_summary2 )



#aplico el modelo a datos nuevos
testing_prediccion  = predict(  modelo, dataset_testing , type = "prob")

# calculo la ganancia normalizada  en testing
ganancia_normalizada2 = fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[ , clase_nomcampo] ) / prob_testing

#---------------------------------
#tres

#genero  datasets de training y testing
set.seed( 133979 )

inTraining        <-  createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]



# generacion del modelo
formula  <-  formula( paste(clase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset_training,   cp=0.005,  xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



#dibujo el arbol en un archivo
jpeg(file = archivo_imagen3,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()

#grabo el summary del modelo
summary( modelo, file= archivo_summary3 )


#aplico el modelo a datos nuevos
testing_prediccion  = predict(  modelo, dataset_testing , type = "prob")

# calculo la ganancia normalizada  en testing
ganancia_normalizada3 = fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[ , clase_nomcampo] ) / prob_testing

#---------------------------------
#cuatro

#genero  datasets de training y testing
set.seed( 999931 )

inTraining        <-  createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]



# generacion del modelo
formula  <-  formula( paste(clase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset_training,   cp=0.005,  xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



#dibujo el arbol en un archivo
jpeg(file = archivo_imagen4,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()

#grabo el summary del modelo
summary( modelo, file= archivo_summary4 )


#aplico el modelo a datos nuevos
testing_prediccion  = predict(  modelo, dataset_testing , type = "prob")

# calculo la ganancia normalizada  en testing
ganancia_normalizada4 = fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[ , clase_nomcampo] ) / prob_testing

#---------------------------------
#cinco

#genero  datasets de training y testing
set.seed( 999763 )

inTraining        <-  createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]



# generacion del modelo
formula  <-  formula( paste(clase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset_training,   cp=0.005,  xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



#dibujo el arbol en un archivo
jpeg(file = archivo_imagen5,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()

#grabo el summary del modelo
summary( modelo, file= archivo_summary5 )


#aplico el modelo a datos nuevos
testing_prediccion  = predict(  modelo, dataset_testing , type = "prob")

# calculo la ganancia normalizada  en testing
ganancia_normalizada5 = fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[ , clase_nomcampo] ) / prob_testing

#---------------------------------

print( c(ganancia_normalizada1, ganancia_normalizada2, ganancia_normalizada3, ganancia_normalizada4, ganancia_normalizada5) )
print( mean( c(ganancia_normalizada1, ganancia_normalizada2, ganancia_normalizada3, ganancia_normalizada4, ganancia_normalizada5) ) )

#notar la dispersion que hay


