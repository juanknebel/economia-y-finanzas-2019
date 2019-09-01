#Objetivo1:  mostrar como la ganancia depende fuertemente de los parametros del rpart
#Objetivo2:  mostrar la imperiosa necesidad de optimizar hiperparametros

#Modelo con libreria  rpart
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   (Monte Carlo Cross Validation)

#source("M:\\R\\elementary\\MiPrimerModelo_03.r")

#limpio la memoria
rm(list=ls())
gc()


library("rpart")
library("data.table")
library("dplyr")
library("rpart.plot")
library("ROCR")


#Parametros entrada
karchivo_entrada      <-  "M:\\datasets\\201902.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)


ktraining_prob        <-  0.70
ksemilla_azar         <-  c(102191, 200177, 410551, 552581, 892237)



#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  19500 
kganancia_noacierto   <-   -500

#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    (+19500) 
#Si NO es acierto  sumar  kganancia_noacierto  (  -500)

fmetrica_ganancia_rpart  = function(probs, clases)
{
 
  return( sum(   (probs > kprob_corte ) * 
                   ifelse(clases== kclase_valor_positivo, kganancia_acierto, kganancia_noacierto)   
             )
        )

}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function(probs, clases)
{
  testing_binaria  <-  as.numeric(clases == kclase_valor_positivo )
  pred             <-  ROCR::prediction( probs, testing_binaria, label.ordering=c(0, 1))
  auc_testing      <-  ROCR::performance(pred,"auc"); 
 
  return(unlist(auc_testing@y.values))

}
#------------------------------------------------------
#Genera el modelo usando una semilla

modelo_rpart_uno = function(psemilla, pmaxdepth, pminbucket, pminsplit, pcp)
{
  #Divido el dataset en training 70% y testing 30%  , usando la libreria dplyr
  dataset  <- as.data.table(mutate(dataset, idtempo =  row_number()))

  set.seed(psemilla )
  dataset_training <- as.data.table(dataset %>%
  group_by(!!as.name(kclase_nomcampo)) %>%
  sample_frac(ktraining_prob) %>%
  ungroup)
  dataset_testing  <- as.data.table(anti_join(dataset, dataset_training, by = "idtempo"))

  dataset_training[ ,  idtempo := NULL    ] 
  dataset_testing[ ,  idtempo := NULL    ] 



  # generacion del modelo
  formula  <-  formula(paste(kclase_nomcampo, "~ ."))

  t0       <-  Sys.time()
  modelo   <-  rpart(formula,   data = dataset_training,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp)
  t1       <-  Sys.time()

  tiempo <-  as.numeric( t1 - t0, units = "secs")


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, dataset_testing , type = "prob")


  # calculo la ganancia normalizada  en testing
  gan <-  fmetrica_ganancia_rpart(testing_prediccion[, kclase_valor_positivo ],  dataset_testing[ , get(kclase_nomcampo)]) / (1- ktraining_prob)

  # calculo el AUC en testing
  auc <- fmetrica_auc_rpart(testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)])



  return( list("semilla"=psemilla, "ganancia"=gan,  "tiempo"= tiempo,  "auc"=auc) )

}

#------------------------------------------------------
#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia = function(dataset, pmaxdepth, pminbucket, pminsplit, pcp )
{

  res  <-   lapply(ksemilla_azar, modelo_rpart_uno, pmaxdepth=pmaxdepth,  pminbucket=pminbucket, pminsplit=pminsplit, pcp=pcp)

  return(rbindlist(res))   

}
#------------------------------------------------------



#cargo los datos
dataset <- fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 


res1  <-  modelo_rpart_ganancia(dataset, pmaxdepth=4, pminbucket=5, pminsplit=20, pcp=0 )
cat( "ganancia_promedio:", mean(res1$ganancia),  "\t",  "AUC_promedio:", mean(res1$auc),  "\n") 

res2  <-  modelo_rpart_ganancia(dataset, pmaxdepth=8, pminbucket=5, pminsplit=20, pcp=0 )
cat( "ganancia_promedio:", mean(res2$ganancia),  "\t",  "AUC_promedio:", mean(res2$auc),  "\n") 

res3  <-  modelo_rpart_ganancia(dataset, pmaxdepth=10, pminbucket=5, pminsplit=20, pcp=0 )
cat( "ganancia_promedio:", mean(res3$ganancia),  "\t",  "AUC_promedio:", mean(res3$auc),  "\n") 

res4  <-  modelo_rpart_ganancia(dataset, pmaxdepth=12, pminbucket=5, pminsplit=20, pcp=0 )
cat( "ganancia_promedio:", mean(res4$ganancia),  "\t",  "AUC_promedio:", mean(res4$auc),  "\n") 

res5  <-  modelo_rpart_ganancia(dataset, pmaxdepth=14, pminbucket=5, pminsplit=20, pcp=0 )
cat( "ganancia_promedio:", mean(res5$ganancia),  "\t",  "AUC_promedio:", mean(res5$auc),  "\n") 

#Por favor notar la gran variabilidad que hay en la ganancia al generar el arbol con distintos parametros
#Cuales seran los parametros que optimizan la ganancia para rpart en este dataset ?
