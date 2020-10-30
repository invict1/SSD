#MODELO 3 

rm( list=ls() )
gc()

library(rpart)
library(caret)

programa             <-  "modelo3.R"
algoritmo            <-  "rpart"
busqueda             <-  "grid"
estimacion           <-  "montecarlo"

archivo_entrada      <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\datasetssd.txt"
campos_separador     <-  "\t"
campo_id             <-  "numero_de_cliente"
clase_nomcampo       <-  "clase_ternaria"
clase_valor_positivo <-  "BAJA+2"
campos_a_borrar      <-  c( )
prob_training        <-  0.7
prob_testing         <-  1 - prob_training
semilla              <-  c( 100069, 100193, 133979, 999931, 999763 )
archivo_salida       <-  "C:\\Users\\invic\\OneDrive\\Escritorio\\SSD\\LaboratorioSSD_Octubre2020\\SalidaModelo3\\salida_3.txt"

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

#calculo inicial de los conjuntos de  <training, testing>
dataset_training  <- list()
dataset_testing   <- list()

for( s in  1:length(semilla) )
{
  set.seed( semilla[s] )
  
  inTraining <- createDataPartition( dataset[ , clase_nomcampo],   p = prob_training, list = FALSE)
  dataset_training[[s]] <- dataset[  inTraining,]
  dataset_testing[[s]]  <- dataset[ -inTraining,]
}

#------------------------------------------------------

rpart_ganancia = function( pmaxdepth, pminbucket, pminsplit, pcp )
{
  
  gan      <- c() 
  tiempo   <- c()
  
  formula  <-  formula( paste(clase_nomcampo, "~ .") )
  
  for( s in  1:length(semilla) )
  {
    # generacion del modelo
    t0       <-  Sys.time()
    modelo   <-  rpart( formula,   data = dataset_training[[s]],  xval=0, maxdepth= pmaxdepth, minbucket= pminbucket, minsplit= pminsplit,  cp= pcp )
    t1       <-  Sys.time()
    
    tiempo[s] <-  as.numeric(  t1 - t0, units = "secs")
    
    
    # aplico el modelo a datos nuevos
    testing_prediccion  <- predict(  modelo, dataset_testing[[s]] , type = "prob")
    
    
    # calculo la ganancia normalizada  en testing
    gan[s] <-  fganancia( testing_prediccion[, clase_valor_positivo ],  dataset_testing[[s]][ , clase_nomcampo] ) / prob_testing
    
  }
  return(  list( "ganancias"=gan, "tiempo"=mean( tiempo) ) )
}

#------------------------------------------------------
#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
  cat( "ganancia", 
       "tiempo_promedio",
       "maxdepth", "minbucket", "minsplit", "cp", 
       "fecha", "dataset", "clase", "programa", "algoritmo", "busqueda" , "estimacion", 
       "gan1", "gan2", "gan3", "gan4", "gan5",
       "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1

linea <- 1

for( vcp  in  c( 0, 0.0005,  0.001, 0.005 ) )
{
  for( vminsplit  in  c( 5, 10, 20, 50, 100, 200, 300 )  )
  {
    for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
    {
      for(  vmaxdepth  in  c( 4, 5, 6, 7, 8, 9, 10, 11, 12 ) )
      {
        
        # manzanita
        #for( vcp  in  c( 0, 0.0001 ) )
        #{
        #for( vminsplit  in  c( 50, 75 )  )
        #{
        #for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
        #{
        #for(  vmaxdepth  in  c(  7,8 ) )
        #{
        
        # anda a saber - 1   
        #for( vcp  in  c( 0, 0.0005, 0.001, 0.005 ) )
        #{
        #for( vminsplit  in  c(  5, 10, 20, 50, 100, 200, 300 )  )
        #{
        #for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
        #{
        #for(  vmaxdepth  in  c(  4, 5, 6, 7, 8, 9, 10, 11, 12 ) )
        #{
        
        # anda a saber - 2      
        #for( vcp  in  c( 0 ) )
        #{
        #for( vminsplit  in  c( 5, 10 )  )
        #{
        #for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
        #{
        #for(  vmaxdepth  in  c( 7, 8, 9, 10, 11 ) )
        #{
        
        # anda a saber - 3   
        #for( vcp  in  c( 0 ) )
        #{
        #for( vminsplit  in  c( 5 )  )
        #{
        #for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
        #{
        #for(  vmaxdepth  in  c( 7, 8, 9 ) )
        #{
        
        # anda a saber - 4   
        #for( vcp  in  c( 0 ) )
        #{
        #for( vminsplit  in  c( 5 )  )
        #{
        #for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
        #{
        #for(  vmaxdepth  in  c( 7 ) )
        #{
        
        if( linea > lineas_archivo )
        {
          gan <- rpart_ganancia( vmaxdepth, vminbucket, vminsplit, vcp  )
          
          cat(  
            mean( gan$ganancias ), 
            gan$tiempo,
            vmaxdepth, 
            vminbucket,
            vminsplit,
            vcp,
            format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, clase_nomcampo, programa, algoritmo, busqueda, estimacion,
            gan$ganancias,
            "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE 
          )
        }
        linea <- linea+1
      }
    }
  }
}

#limpio la memoria
rm( list=ls() )
gc()

quit( save="no" )
