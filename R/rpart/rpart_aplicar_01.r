#Aplicacion del modelo de rpart

#Aqui la ganancia NO se estima, sino que se calcula directamente. 
#No hay training/testing

#Se trabaja con estos hiperparametros fijos de rpart, los que obviamente en algun momento se optimizaran
#  vxval       <-   0
#  vmaxdepth   <-  16
#  vminbucket  <-   6
#  vminsplit   <-  20 
#  vcp         <-   0

#Se aplica  rpart con los anteriores parametros a varios meses de generacion y aplicacion
#se trabaja con los datasets de fechas relativas en dias


#source("~/cloud/cloud1/R/rpart/rpart_aplicar_01.r")

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



data <- list()
data$campos_separador     <-  "\t"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c(data$campo_id)
env$data <-  data


problema <- list()
problema$prob_corte           <-      0.025
problema$ganancia_acierto     <-  19500 
problema$ganancia_noacierto   <-   -500

env$problema <- problema

#Hacer que la variable   env   NO se pueda modificar
lockBinding( "env", globalenv() )


#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == pclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------
pst_mesgeneracion <- "201902_dias.txt"
pst_mesevaluacion <- "201904_dias.txt"

faplicar_modelo  = function(pst_mesgeneracion, pst_mesevaluacion)
{

  varchivos_train  <-  unlist(strsplit(pst_mesgeneracion, split=", "))
  #cargo los datos de TODOS los archivos de la lista  karchivos_train
  setwd(env$directory$datasets)
  dataset_generacion <- do.call(rbind, lapply(varchivos_train, function(x) fread(x, header=TRUE, sep=env$data$campos_separador)))
 


  #borro las variables que no me interesan
  dataset_generacion[ ,  (env$data$campos_a_borrar) := NULL    ] 


  #estos son los valores que en la corrida optimizacion de hiperparametros dan la mayor ganancia
  vxval       <-   0
  vmaxdepth   <-  16
  vminbucket  <-   6
  vminsplit   <-  20 
  vcp         <-   0


  #notar que el modelo se genera utilizando TODO el dataset
  formula  <-  formula(paste(env$data$clase_nomcampo, "~ ."))
  modelo   <-  rpart(formula,   data = dataset_generacion,  
                      xval=vxval, maxdepth=vmaxdepth, minbucket=vminbucket, minsplit=vminsplit, cp=vcp)


  #------------------------
  #ahora paso a aplicar el modelo que recien genere

  #cargo los datos de TODOS los archivos de la lista  pst_mesevaluacion
  setwd(env$directory$datasets)
  varchivos_aplicar  <-  unlist(strsplit(pst_mesevaluacion, split=", "))
  dataset_aplicacion <-  do.call(rbind, lapply(varchivos_aplicar, function(x) fread(x, header=TRUE, sep=env$data$campos_separador)))
 


  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict( modelo, dataset_aplicacion , type = "prob")


  # calculo la ganancia en los datos de prediccion
  gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion[, env$data$clase_valor_positivo ],  
                                   dataset_aplicacion[ , get(env$data$clase_nomcampo)], 
                                   env$data$clase_valor_positivo,
                                   env$problema )

  auc <-  fmetrica_auc_rpart( aplicacion_prediccion[, env$data$clase_valor_positivo ],
                              dataset_aplicacion[ , get(env$data$clase_nomcampo)], 
                              env$data$clase_valor_positivo )


  return(list("ganancia"=gan,  "auc"=auc, "eval"=pst_mesevaluacion, "generacion"=pst_mesgeneracion )) 
}
#------------------------------------------------------------------------------

#leo el plan de donde voy a generar el modelo y a que meses lo voy a aplicar para medir la ganancia

generacion <- c( "201902_dias.txt", 
                 "201901_dias.txt",
                 "201812_dias.txt",
                 "201902_dias.txt, 201901_dias.txt, 201812_dias.txt",
                 "201902_dias.txt, 201901_dias.txt, 201812_dias.txt, 201811_dias.txt, 201810_dias.txt, 201809_dias.txt",
                 "201902_dias.txt, 201901_dias.txt, 201812_dias.txt, 201811_dias.txt, 201810_dias.txt, 201809_dias.txt, 201808_dias.txt, 201807_dias.txt, 201806_dias.txt, 201805_dias.txt, 201804_dias.txt, 201803_dias.txt",
                 "201802_dias.txt",
                 "201802_dias.txt, 201801_dias.txt, 201712_dias.txt, 201711_dias.txt, 201710_dias.txt, 201709_dias.txt"
              )

aplicacion <- c( "201904_dias.txt", 
                 "201904_dias.txt",
                 "201904_dias.txt",
                 "201904_dias.txt",
                 "201904_dias.txt",
                 "201904_dias.txt",
                 "201804_dias.txt",
                 "201804_dias.txt"
              )


res <- mapply( faplicar_modelo,  generacion,  aplicacion)

res2 <-  as.data.table(t(res))

res2

