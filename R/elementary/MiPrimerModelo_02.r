#Objetivo:  mostrar  Repeated Random Sub Sampling Validation 

#Modelo con libreria  rpart
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   (Monte Carlo Cross Validation)

#source("M:\\R\\elementary\\MiPrimerModelo_02.r")

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

#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia = function(dataset )
{
  gan      <- c()
  auc      <- c() 
  tiempo   <- c()

  cantidad_semillas <- length(ksemilla_azar)
	
  for(s in  1:cantidad_semillas)
  {

    #Divido el dataset en training 70% y testing 30%  , usando la libreria dplyr
    dataset  <- as.data.table(mutate(dataset, idtempo =  row_number()))

    set.seed(ksemilla_azar[s] )
    dataset_training <- as.data.table(dataset %>%
  group_by(!!as.name(kclase_nomcampo)) %>%
  sample_frac(    ktraining_prob) %>%
  ungroup)
    dataset_testing  <- as.data.table(anti_join(dataset, dataset_training, by = "idtempo"))

    dataset_training[ ,  idtempo := NULL    ] 
    dataset_testing[ ,  idtempo := NULL    ] 



    # generacion del modelo
    formula  <-  formula(paste(kclase_nomcampo, "~ ."))

    t0       <-  Sys.time()
    modelo   <-  rpart(formula,   data = dataset_training,   cp=0.0,  xval=0)
    t1       <-  Sys.time()

    tiempo[s] <-  as.numeric( t1 - t0, units = "secs")


    #aplico el modelo a datos nuevos
    testing_prediccion  <- predict( modelo, dataset_testing , type = "prob")


    # calculo la ganancia normalizada  en testing
    gan[s] <-  fmetrica_ganancia_rpart(testing_prediccion[, kclase_valor_positivo ],  dataset_testing[ , get(kclase_nomcampo)]) / (1- ktraining_prob)

    # calculo el AUC en testing
    auc[s]     <- fmetrica_auc_rpart(testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)])

  }


   return( list("ganancia_promedio"=mean(gan),  "vganancias"=gan ,  "vtiempos"= tiempo,  "AUC_promedio"=mean(auc), "vAUCs"=auc) )

}
#------------------------------------------------------



#cargo los datos
dataset <- fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 


res  <-  modelo_rpart_ganancia(dataset)

print(res)


