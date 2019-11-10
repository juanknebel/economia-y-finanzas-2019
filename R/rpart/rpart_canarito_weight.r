#Arbol con libreria  rpart
#rpart  sin hiperparametros, NO HACE FALTA OPTIMIZAR NADA
#no se hace training/testing  ni Montecarlo ni Cross-Validation
#se agregan canaritos, un 20% de la cantidad devariables y se lo deja crecer al arbol hasta donde pueda
#luego, se poda el arbol donde hay canaritos

#"un patron es lo que no es azar"

#atencion  rpart toma malas decisiones si se deja minbucket=1 por lo que se lo establecio en 5

#atencion, NO se hace regularizacion
#por lo que la probabilidad que devuelve NO ES DEL TODO BUENA

#entreno en 201902_dias.txt
#aplico el modelo a  201904_dias.txt  datos que NO fueron vistos durante el entrenamiento

#source("M:\\R\\rpart\\rpart_canarito_weight.r")

#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("rpart")
library("rpart.plot")

directory <-list()
switch (Sys.info()[['sysname']],
         Windows = { 
                     directory$work     <-  "M:\\work\\"
                     directory$plan     <-  "M:\\plan\\"
                     directory$datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { 
                     directory$work     <-  "~/dm/work/"
                     directory$plan     <-  "~/dm/plan/"
                     directory$datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { 
                     directory$work     <-  "~/cloud/cloud1/work/"
                     directory$plan     <-  "~/cloud/cloud1/plan/"
                     directory$datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
       )


#Parametros entrada
karchivo_actual       <-  "201902_dias.txt"
karchivo_futuro       <-  "201904_dias.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)
kprob_corte           <-  0.025
kganancia_acierto     <-  19500
kganancia_noacierto   <-   -500

#Parametros salida
karchivo_salida        <-  "salida.txt"
karchivo_imagen        <-  "rpartw_canarito.jpg"
karchivo_imagen_pruned <-  "rpartw_canarito_pruned.jpg"

#------------------------------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +19500 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -500 )

fmetrica_ganancia_rpart  = function( probs, clases, pclase_valor_positivo, pprob_corte )
{
 
  return(  sum(    (probs > pprob_corte  ) * 
                   ifelse( clases== kclase_valor_positivo, kganancia_acierto, kganancia_noacierto )   
              )
         )
}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == kclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------
#Inicio del programa
t0       <-  Sys.time()

#cargo los datos
setwd( directory$datasets )
dataset_actual  <- fread(karchivo_actual)
dataset_futuro  <- fread(karchivo_futuro)

#borro las variables que no me interesan
dataset_actual[ ,  (kcampos_a_borrar) := NULL    ]
dataset_futuro[ ,  (kcampos_a_borrar) := NULL    ]

dataset_actual[ clase_ternaria=="BAJA+1" , clase_ternaria:= "CONTINUA" ]
dataset_futuro[ clase_ternaria=="BAJA+1" , clase_ternaria:= "CONTINUA" ]


#----------------------------------------
#agrego las variables canarito
#canarito_idx  es el indice de canaritos,  0.2 significa agregar tantos canaritos como el 20% de los campos del dataset
canaritos_idx <- 0.50
canaritos_cantidad <- as.integer( round(ncol(dataset_actual) * canaritos_idx) )
vcanaritos <-  paste0( "canarito", 1:canaritos_cantidad )


#uso esta semilla para los canaritos
set.seed(10219)

#podria haber hecho un loop for
dataset_actual[ , (vcanaritos) := 0 ]
dataset_actual[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]
dataset_futuro[ , (vcanaritos) := 0 ]
dataset_futuro[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]

#ahora hago que los canaritos sean las primeras variables del dataset
nuevo_orden <-  c( vcanaritos, setdiff( colnames( dataset_actual), vcanaritos )) 
setcolorder( dataset_actual, nuevo_orden )
setcolorder( dataset_futuro, nuevo_orden )

#---------------------------------------

t0       <-  Sys.time()

# generacion del modelo lo dejo CRECER LIBRE
#hago  oversampling de los positivos
vpeso_positivos <- 20
vpesos <-  ifelse( dataset_actual$clase_ternaria=="BAJA+2", vpeso_positivos, 1 )

#Acomodo la probabilidad de corte
vprob_corte     <- - kganancia_noacierto*vpeso_positivos/( kganancia_acierto - kganancia_noacierto*vpeso_positivos )

formula  <-  formula(paste(kclase_nomcampo, "~ ."))
modelo_original   <-  rpart(formula,   data = dataset_actual,   weights=vpesos,  xval=0, cp=0.0, maxdepth=30, minbucket=10, minsplit=10 )

#calculo unas metricas del arbol original
#calculo la profundidad del arbol
profundidad_original  <-  max(rpart:::tree.depth(as.numeric(rownames(modelo_original$frame))))

#cuento cuantas variables canarito distintas aparecen
frame  <- modelo_original$frame
leaves <- frame$var == "<leaf>"
used   <- unique(frame$var[!leaves])
canaritos_muertos_original <- sum( unlist( used ) %like% "canarito" )


#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -100 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -100
modelo_pruned <- prune(  modelo_original, -100.0 )

#calculo la profundidad del arbol pruned
profundidad_pruned  <-  max(rpart:::tree.depth(as.numeric(rownames(modelo_pruned$frame))))

#cuento cuantas variables canarito distintas aparecen en el arbol pruned
frame  <- modelo_pruned$frame
leaves <- frame$var == "<leaf>"
used   <- unique(frame$var[!leaves])
canaritos_muertos_pruned <- sum( unlist( used ) %like% "canarito" )


#aplico los modelos a datos nuevos
testing_prediccion_original  <- predict( modelo_original, dataset_futuro, type = "prob")
testing_prediccion_pruned    <- predict( modelo_pruned,   dataset_futuro, type = "prob")


# calculo la ganancia normalizada  en testing
gan_original  <-  fmetrica_ganancia_rpart(testing_prediccion_original[, kclase_valor_positivo ],  dataset_futuro[, get(kclase_nomcampo)], kclase_valor_positivo, vprob_corte )
gan_pruned    <-  fmetrica_ganancia_rpart(  testing_prediccion_pruned[, kclase_valor_positivo ],  dataset_futuro[, get(kclase_nomcampo)], kclase_valor_positivo, vprob_corte )
# calculo el AUC en testing
auc_original  <-  fmetrica_auc_rpart(testing_prediccion_original[, kclase_valor_positivo ],  dataset_futuro[, get(kclase_nomcampo)], kclase_valor_positivo )
auc_pruned    <-  fmetrica_auc_rpart(  testing_prediccion_pruned[, kclase_valor_positivo ],  dataset_futuro[, get(kclase_nomcampo)], kclase_valor_positivo )


#imprimo los resultados
#Obviamente espero que el modelo_original  al dejarlo crecer sin control, cometa mucho overfitting
cat(  "modelo original",
      "\tcanaritos_idx:", canaritos_idx, 
      "\tmuertos:", canaritos_muertos_original,
      "\tgan:", gan_original, 
      "\tAUC:", auc_original, 
      "\tprof:", profundidad_original, 
      "\n")

cat(  "modelo pruned",
      "\tcanaritos_idx:", canaritos_idx, 
      "\tmuertos:", canaritos_muertos_pruned,
      "\tgan:", gan_pruned, 
      "\tAUC:", auc_pruned, 
      "\tprof:", profundidad_pruned, 
      "\n")


t1       <-  Sys.time()
tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")
tiempo_corrida

#paso a imprimir las imagenes de los arboles

#imprimo el arbol original, que deberia ser una monstruosidad
setwd( directory$work )
jpeg(file = karchivo_imagen,  width = 100, height = 12, units = 'in', res = 300)
prp(modelo_original, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

#imprimo el arbol podado, que no deberia tener ningun canarito
setwd( directory$work )
jpeg(file = karchivo_imagen_pruned,  width = 40, height = 5, units = 'in', res = 300)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

