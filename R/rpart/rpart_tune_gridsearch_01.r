#Modelo con libreria  rpart con busqueda  Grid Search
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   (Monte Carlo Cross Validation)
#Por favor, no desesperarse por la ESPANTOSA granularidad de la Grid Search
#notar el uso de CPU y memoria RAM

#Si este programa se corta,  se lo debe volver a correr y automaticamente retoma desde donde llego la vez anterior

#source("~/cloud/cloud1/R/rpart/rpart_tune_gridsearch_01.r")


#limpio la memoria
rm(list=ls())
gc()


library("rpart")
library("data.table")


#raiz del environment
env <- list()

directory <-list()
switch (Sys.info()[['sysname']],
         Windows = { directory$work     <-  "M:\\work\\"
                     directory$plan     <-  "M:\\plan\\"
                     directory$datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { directory$work     <-  "~/dm/work/"
                     directory$plan     <-  "~/dm/plan/"
                     directory$datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { directory$work     <-  "~/cloud/cloud1/work/"
                     directory$plan     <-  "~/cloud/cloud1/plan/"
                     directory$datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
       )
env$directory <- directory


#Parametros entrada de nuestro dataset
data <- list()
data$archivo_actual       <-  "201902_dias.txt"
data$archivo_futuro_train <-  "201902_dias.txt"
data$archivo_futuro_test  <-  "201904_dias.txt"

data$campos_separador     <-  "\t"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c(data$campo_id)
env$data  <-  data


#Parametros  Repeated Random Sub Sampling Validation
montecarlo <- list()
montecarlo$training_prob  <-  0.70
montecarlo$semilla_azar   <-  c(102191, 200177, 410551, 552581, 892237)
env$montecarlo  <-  montecarlo


#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento          <-  1105
hiper$arch_global          <-  "hiperparametro_GLOBAL.txt"
hiper$clase                <-  "ternaria"
hiper$programa             <-  "rpart_tune_gridsearch_01.r"
hiper$algoritmo            <-  "rpart"
hiper$busqueda             <-  "gridsearch"
hiper$estimacion           <-  "Montecarlo"
hiper$observaciones        <-  "5 semillas"

env$hiper <-  hiper

problema <- list()
problema$prob_corte           <-      0.025
problema$ganancia_acierto     <-  19500 
problema$ganancia_noacierto   <-   -500

env$problema <- problema

#Hacer que la variable   env   NO se pueda modificar
lockBinding( "env", globalenv() )

#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +19500 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -500 )

fmetrica_ganancia_rpart  = function( probs, clases, pclase_valor_positivo, problema )
{
 
  return(  sum(    (probs > problema$prob_corte  ) * 
                   ifelse( clases== pclase_valor_positivo, problema$ganancia_acierto, problema$ganancia_noacierto )   
              )
         )
}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == pclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------
#Genera el modelo usando una semilla

modelo_rpart = function( ptrain, ptest, pmaxdepth, pminbucket, pminsplit, pcp )
{

  formula  <-  formula(paste(env$data$clase_nomcampo, "~ ."))

  modelo   <-  rpart(formula,   data = ptrain,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp)


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest, type = "prob")


  # calculo la ganancia normalizada  en testing
  gan  <-  fmetrica_ganancia_rpart(testing_prediccion[, env$data$clase_valor_positivo ],  ptest[, get(env$data$clase_nomcampo)], env$data$clase_valor_positivo, env$problema)
  # calculo el AUC en testing
  auc  <-  fmetrica_auc_rpart(testing_prediccion[, env$data$clase_valor_positivo ],  ptest[, get(env$data$clase_nomcampo)], env$data$clase_valor_positivo )

  #cuento cuantas variables canarito distintas aparecen
  frame  <- modelo$frame
  leaves <- frame$var == "<leaf>"
  used   <- unique(frame$var[!leaves])
  canaritos_muertos <- sum( unlist( used ) %like% "canarito" )


  return( list("ganancia_test"=gan, "auc_test"=auc, "canaritos_muertos"=canaritos_muertos) )
}

#------------------------------------------------------

modelo_rpart_semilla = function( psemilla, pdataset, pmaxdepth, pminbucket, pminsplit, pcp )
{

  #Divido el dataset en training 70% y testing 30%
  set.seed( psemilla )
  tb_clase <-  as.data.table(  pdataset[ , get(env$data$clase_nomcampo) ] )
  colnames( tb_clase ) <- c("clase" )
  tb_clase[  , c("azar","idtempo" ) := list( runif( nrow(tb_clase) ), .I ) ]
  
  setorder( tb_clase, clase, azar )
  tb_clase[  , training := (.I%%10)<7 ]
  
  setorder( tb_clase, idtempo )
  pdataset[  , training:=  tb_clase$training ]
  
  dataset_training <- pdataset[ (training) ]
  dataset_testing  <- pdataset[ !(training) ]
  
  
  res <-  modelo_rpart( dataset_training, dataset_testing, pmaxdepth, pminbucket, pminsplit, pcp )

  rm( tb_clase, dataset_training, dataset_testing )
  gc()

  return( res )
}

#
#------------------------------------------------------
#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia = function( pdataset_actual, pdataset_futuro_train, pdataset_futuro_test, pmaxdepth, pminbucket, pminsplit, pcp )
{

  t0       <-  Sys.time()
  #Genero los 5 modelos que resultan de 5 training/testing,  devuelvo medicion en testing
  res_actual  <-   lapply(env$montecarlo$semilla_azar, modelo_rpart_semilla, 
                          pdataset=dataset_actual, 
                          pmaxdepth=pmaxdepth,  pminbucket=pminbucket, pminsplit=pminsplit, pcp=pcp)

  res_actual <- rbindlist( res_actual )


  res_futuro <- modelo_rpart( pdataset_futuro_train, pdataset_futuro_test, pmaxdepth, pminbucket, pminsplit, pcp )


  t1       <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")  


  st_parametros = paste("xval=",      0,          ", " ,
                        "maxdepth=",  pmaxdepth,  ", " ,
                        "minbucket=", pminbucket, ", " ,
                        "minsplit=",  pminsplit,  ", " ,
                        "cp=",        pcp,
                        sep = ""
                       )

  cat( env$hiper$experimento, 
       mean(res_actual$ganancia_test)/( 1 - env$montecarlo$training_prob), #ganancia en testing NORMALIZADA
       mean(res_actual$auc_test),
       mean(res_actual$canaritos_muertos),
       res_futuro$ganancia_test,
       res_futuro$auc_test,
       mean(res_futuro$canaritos_muertos),
       tiempo_corrida,
       env$problema$prob_corte,
       st_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       env$hiper$clase, 
       env$hiper$programa, 
       env$hiper$algoritmo, 
       env$hiper$busqueda, 
       env$hiper$estimacion,
       env$data$archivo_actual, 
       env$data$archivo_actual, 
       env$data$archivo_futuro_train,
       env$data$archivo_futuro_test,
       env$hiper$observaciones,
       "\n", sep="\t", 
       file=env$hiper$arch_global, 
       fill=FALSE, append=TRUE 
     )
}
#------------------------------------------------------


tipo_os <- Sys.info()[['sysname']]

#cargo los datos
setwd( env$directory$datasets )
if( tipo_os=="Linux" ) { dataset_actual  <- fread(cmd=paste( "cat ", env$data$archivo_actual)) } else 
{ dataset_actual  <- fread(env$data$archivo_actual) }

#borro las variables que no me interesan
dataset_actual[ ,  (env$data$campos_a_borrar) := NULL    ] 


setwd( env$directory$datasets )
if( tipo_os=="Linux" ) { dataset_futuro_train  <- fread(cmd=paste( "cat ", env$data$archivo_futuro_train)) } else 
{ dataset_futuro_train  <- fread(env$data$archivo_futuro_train) }

#borro las variables que no me interesan
dataset_futuro_train[ ,  (env$data$campos_a_borrar) := NULL    ] 


setwd( env$directory$datasets )
if( tipo_os=="Linux" ) { dataset_futuro_test  <- fread(cmd=paste( "cat ", env$data$archivo_futuro_test)) } else 
{ dataset_futuro_test  <- fread(env$data$archivo_futuro_test) }

#borro las variables que no me interesan
dataset_futuro_test[ ,  (env$data$campos_a_borrar) := NULL    ] 

#creo los campos canarito
magic_canaritos <- 0.1 
canaritos_cantidad <- as.integer( round(ncol(dataset_actual) * magic_canaritos) )
for( i in 1:canaritos_cantidad )
{
  dataset_actual[        , paste0( "canarito", i ) :=  runif( nrow(dataset_actual) ) ]
  dataset_futuro_train[  , paste0( "canarito", i ) :=  runif( nrow(dataset_futuro_train) ) ]
  dataset_futuro_test[   , paste0( "canarito", i ) :=  runif( nrow(dataset_futuro_test) ) ] 
}



#escribo los  titulos  del archivo salida
setwd( env$directory$work )
if(!file.exists( env$hiper$arch_global))
{
  cat("experimento",
      "metrica1_actual",
      "metrica2_actual",
      "canaritos_muertos_actual",
      "metrica1_futuro",
      "metrica2_futuro",
      "canaritos_muertos_futuro",
      "tiempo",
      "pcorte",
      "parametros",
      "fecha", 
      "clase", "programa", "algoritmo", "busqueda" , "estimacion",
      "dataset_actual_train", "dataset_actual_test", "dataset_futuro_train", "dataset_futuro_test",
      "observaciones",
      "\n", sep="\t", 
      file=env$hiper$arch_global, 
      fill=FALSE, append=FALSE)
  
   
  lineas_salida <- 0
} else
{
  salida <-  read.table( env$hiper$arch_global, header=TRUE, sep=env$data$campos_separador)
  salida <-  subset( salida, experimento== env$hiper$experimento ) 
  lineas_salida <- nrow(salida)
}



linea <- 1

for(vcp  in  c( 0, 0.0001, 0.0005,  0.001, 0.005))
{
for(vminsplit  in  c( 2, 5, 10, 20, 50, 100, 200) )
{
for(vminbucket  in  c(trunc(vminsplit/5), trunc(vminsplit/4), trunc(vminsplit/3)) )
{
for( vmaxdepth  in  c( 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30))
{ 

  if(linea > lineas_salida)  #no volver a procesar si en la corrida anterior se llego a esa lina
  {

    res <- modelo_rpart_ganancia( dataset_actual, dataset_futuro_train, dataset_futuro_test, 
                                  pmaxdepth=vmaxdepth, pminbucket=vminbucket, pminsplit=vminsplit, pcp=vcp )

   
  }

  linea <- linea+1

}
}
}
}



#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

