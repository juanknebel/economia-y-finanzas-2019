#Aplicacion del modelo de rpart

#Aqui la ganancia NO se estima, sino que se calcula directamente. 
#No hay training/testing


#source("~/cloud/cloud1/R/inicial/Modelo_aplicar_02.r")

#limpio la memoria
rm(list=ls())
gc()





library("rpart")
library("data.table")




#Parametros entrada de nuestro dataset
karchivo_generacion   <-  "~/cloud/cloud1/datasets/dias/201902_dias.txt"
karchivo_aplicacion   <-  "~/cloud/cloud1/datasets/dias/201904_dias.txt"

kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)


karchivo_intermedio   <-  "Modelo_aplicar_intermedio.txt"

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


#cargo los datos de generacion
dataset_generacion <- fread(cmd=paste("cat",karchivo_generacion), header=TRUE, sep=kcampos_separador)


#borro las variables que no me interesan
dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 


#estos son los parametros de un modelo bueno
vxval       <-   0
vmaxdepth   <-  16
vminbucket  <-   6
vminsplit   <-  20 
vcp         <-   0


#notar que el modelo se genera utilizando TODO el dataset
formula  <-  formula(paste(kclase_nomcampo, "~ ."))
modelo   <-  rpart(formula,   data = dataset_generacion,  
                    xval=vxval, maxdepth=vmaxdepth, minbucket=vminbucket, minsplit=vminsplit, cp=vcp)


#------------------------
#ahora paso a aplicar el modelo que recien genere

#cargo los datos de aplicacion
dataset_aplicacion <- fread(karchivo_aplicacion, header=TRUE, sep=kcampos_separador)


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict( modelo, dataset_aplicacion , type = "prob")



#-----------------------
#Las siguientes lineas son para mostrar el detalle de la prediccion y el calculo de la ganancia
#agrego la columno  numero_de_cliente y la clase
data_original <-   as.data.table(cbind(dataset_aplicacion[ , get(kcampo_id)],  dataset_aplicacion[ , get(kclase_nomcampo)  ]))
colnames(data_original) <-  c(kcampo_id, kclase_nomcampo) 

#para facilitarme la vida, agrego el campo positivo
data_original[  , positivo:= as.numeric(get(kclase_nomcampo)== kclase_valor_positivo) ]


#pego el ID y la clase con  la matrix de las predicciones
data_pred      <-  as.data.table(cbind(  data_original,  aplicacion_prediccion))


#calculo si envio o no a la campana de retencion
data_pred[  , enviar := as.numeric(get(kclase_valor_positivo) > kprob_corte) ]   #kprob_corte=0.025 0 si no envio, 1 si envio


#hago unos calculos a ver cuantos envio a la campana de retencion
nrow(data_pred)
sum( data_pred$enviar)
sum( data_pred$enviar) / nrow(data_pred)


data_pred[  , ganancia :=  ifelse( enviar, ifelse(positivo, kganancia_acierto, kganancia_noacierto), NA) ]    #kganancia_acierto=11700

ganancia_manual <- sum(data_pred$ganancia, na.rm=TRUE)

envios_marketing      <-  sum( data_pred$enviar)
aciertos_marketing    <-  sum( data_pred[ , enviar*positivo] )
noaciertos_marketing  <-  sum( data_pred[ , (1-enviar)*positivo]) 

ganancia_manual2      <-  aciertos_marketing*kganancia_acierto +  noaciertos_marketing*kganancia_noacierto

#Que llamativo, en mi campana de marketing le estoy acertando a menos del 10% 
indice_aciertos       <-   aciertos_marketing/envios_marketing


true_positive         <-  sum(data_pred[ , enviar*positivo], na.rm=TRUE)
false_positive        <-  sum(data_pred[ , enviar*(1-positivo)], na.rm=TRUE) 
false_negative        <-  sum(data_pred[ , (1-enviar)*positivo], na.rm=TRUE)
true_negative         <-  sum(data_pred[ , (1-enviar)*(1-positivo)], na.rm=TRUE)

densidad_universo     <-  sum(data_pred$positivo) / nrow( data_pred)
densidad_envio        <-  sum(data_pred[ , enviar*positivo]) / sum(  data_pred$enviar)
lift_envio            <-  densidad_envio / densidad_universo

#-----------------------


#continuo con el codigo normal

# calculo la ganancia en los datos de prediccion
gan <-  fmetrica_ganancia_rpart(aplicacion_prediccion[, kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)])


# calculo el AUC en aplicacion
auc <-  fmetrica_auc_rpart(aplicacion_prediccion[, kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)])


