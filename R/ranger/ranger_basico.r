
#source( "/cloud/cloud1/codigoR/ranger/ranger_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()





switch ( Sys.info()[['sysname']],
         Windows = { directory.work     <-  "M:\\work\\"
                     directory.datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { directory.work     <-  "~/dm/work/"
                     directory.datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { directory.work     <-  "~/cloud/cloud1/work/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
        )



library( "ranger" )
library( "randomForest" )  #solo se usa para imputar nulos

library( "data.table" )



kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )

karchivo_generacion   <-   "201902_dias.txt,201901_dias.txt,201812_dias.txt"
karchivo_aplicacion   <-   "201904_dias.txt"


library( "ROCR" )


#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  19500 
kganancia_noacierto   <-   -500

#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +11700 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -300 )

fmetrica_ganancia_rpart  = function( probs, clases )
{
 
  return(  sum(    (probs > kprob_corte  ) * 
                   ifelse( clases== kclase_valor_positivo, kganancia_acierto, kganancia_noacierto )   
              )
         )

}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases )
{
  testing_binaria  <-  as.numeric( clases == kclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

setwd(  directory.datasets )
varchivos_generacion  <-  unlist(strsplit( karchivo_generacion, split=","))
dataset_generacion    <-  do.call(rbind, lapply( varchivos_generacion, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))


#borro las variables que no me interesan
dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 

#imputo los nulos, ya que ranger no acepta nulos
dataset_generacion <-  na.roughfix( dataset_generacion )



#-------------------------
#genero el modelo
formula  <-  formula( paste(kclase_nomcampo, "~ .") )

t0       <-  Sys.time()

modelo  <- ranger( data = dataset_generacion,  
                   formula,  
                   probability=TRUE,
                   num.trees= 900, 
                   min.node.size= 360, 
                   mtry= 4, 
                   splitrule='gini'
                 )       

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")


#-------------------------
#aplico el modelo

setwd(  directory.datasets )
dataset_aplicacion <-  fread( karchivo_aplicacion, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador) 

#borro las variables que no me interesan
dataset_aplicacion[ ,  (kcampos_a_borrar) := NULL    ] 

#imputo los nulos, ya que ranger no acepta nulos
dataset_aplicacion <-  na.roughfix( dataset_aplicacion )


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, dataset_aplicacion )



# calculo la ganancia
# notar que el resultado queda en   aplicacion_prediccion$predictions 

gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion$predictions[ ,kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] ) 

# calculo el AUC
auc <-  fmetrica_auc_rpart( aplicacion_prediccion$predictions[ ,kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] ) 


cat( "ganancia = ",  gan , "\n")
cat( "AUC = ",  auc, "\n"  )
cat( "tiempo = ",  tiempo, "\n"  )




