#Este codigo es solamente valido para la UBA

#Este programa hace ONCE optimizaciones bayesianas, y usa el archivo  _hist.txt
#se trabaja  con  1={BAJA+1, BAJA+2}  0={CONTINUA}
#se calcula la nueva probabilidad de corte, sobre la mitad al azar de testing

#Atencion, este programa NO TRABAJA  con BAJA+3, BAJA+4, etc

#seria muy deseable utilizar el dataset  _exthist.txt  generado por  fe_todoenuno.r


#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("ROCR")
library("lightgbm")

library("DiceKriging")
library("mlrMBO")


#raiz del environment
env <- list()

directory <-list()
switch (Sys.info()[['sysname']],
         Windows = { 
                     directory$work     <-  "M:\\work\\"
                     directory$datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { 
                     directory$work     <-  "~/dm/work/"
                     directory$datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { 
                     directory$work     <-  "~/cloud/cloud1/work/"
                     directory$datasets <-  "~/cloud/cloud1/datasets/"
                   }
       )
env$directory <- directory


env$experimento          <-  16101
env$procesar_solo_201904 <-  FALSE

env$undersampling        <-  0.1


#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$clase_valor_negativo <-  "CONTINUA"
data$campos_a_borrar      <-  c()
data$archivo_grande       <-  "paquete_premium_hist.txt.gz"  #aqui deberia usar el  _exthist  que tiene todos los campos creativos
env$data <- data


#Parametros  mlrMBO   Optimizacion Bayesiana
mbo <- list()
mbo$iteraciones       <-  100   # cuantas iteraciones hace la Optimizacion Bayesiana
mbo$saveondisk_time   <-  600   # cada 600 segundos guarda a disco cuanto avanzo
env$mbo <- mbo

#sobre el funcionamiento de programa
#en el lightgbm
lgb <- list()
lgb$semilla               <- 102191
lgb$max_bin               <-    255
lgb$subsample             <-      1.0
lgb$num_iterations_max    <-   1000
lgb$early_stopping_round  <-     30
lgb$num_leaves            <-   1024
env$lightgbm <- lgb



#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory$work
hiper$clase_tipo     <-  "binaria1"
hiper$programa       <-  "lightgbm_directo_wfv.r"
hiper$algoritmo      <-  "lightgbm"
hiper$busqueda       <-  "MBO"
hiper$estimacion     <-  "mes_futuro"
hiper$observaciones  <-  "ventana_undersampling_10%"
hiper$separador      <-  "\t"
hiper$directory      <-  env$directory$work
env$hiper <-  hiper


problema <- list()
problema$ganancia_acierto     <-  19500 
problema$ganancia_noacierto   <-   -500
env$problema <- problema


#Hacer que la variable   env   NO se pueda modificar
lockBinding( "env", globalenv() )



#------------------------------------------------------

hiperparametros_titulos <- function( phiper, parchivo )
{
  #escribo los  titulos  del archivo salida
  setwd(phiper$directory)
  if(!file.exists(parchivo))
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
        "dataset_actual_train", "dataset_actual_test", "dataset_futuro_train", "dataset_futuro_test", "observaciones",
        "\n", 
        sep=phiper$separador, 
        file=parchivo, 
        fill=FALSE, 
        append=FALSE)
  }

}
#------------------------------------------------------------------------------

hiperparametros_crear <- function( phiper)
{
  hiperparametros_titulos( phiper, phiper$arch_global ) 
  hiperparametros_titulos( phiper, phiper$arch_local ) 
}
#------------------------------------------------------


hiperparametros_grabar1 <- function( phiper, parchivo,
                                    pactual_ganancia, pactual_auc, pactual_canaritos, 
                                    pfuturo_ganancia, pfuturo_auc, pfuturo_canaritos, 
                                    ptiempo, pprob_corte, pst_parametros, 
                                    parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase)
{

  setwd(phiper$directory)

  cat( phiper$experimento,
       pactual_ganancia, 
       pactual_auc,
       pactual_canaritos,
       pfuturo_ganancia, 
       pfuturo_auc,
       pfuturo_canaritos,
       ptiempo,
       pprob_corte,
       pst_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       ifelse( is.na( pclase), phiper$clase_tipo,  pclase ),
       phiper$programa, 
       phiper$algoritmo, 
       phiper$busqueda, 
       phiper$estimacion,
       parchivo_actual_train, parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test,
       phiper$observaciones,
       "\n", 
      sep=phiper$separador, 
      file=parchivo, 
      fill=FALSE, append=TRUE 
    )

}
#------------------------------------------------------------------------------

hiperparametros_grabar <- function( phiper,
                                   pactual_ganancia, pactual_auc, pactual_canaritos, 
                                   pfuturo_ganancia, pfuturo_auc, pfuturo_canaritos, 
                                   ptiempo, pprob_corte, pst_parametros, 
                                   parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase,
                                   pcanaritos_muertos)
{
  #grabo el archivo  GLOBAL
  hiperparametros_grabar1(phiper, phiper$arch_global,
                          pactual_ganancia, pactual_auc, pactual_canaritos, 
                          pfuturo_ganancia, pfuturo_auc, pfuturo_canaritos,
                          ptiempo, pprob_corte, pst_parametros, 
                          parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase )

  #grago el archivo  LOCAL
  hiperparametros_grabar1(phiper, phiper$arch_local,
                          pactual_ganancia, pactual_auc, pactual_canaritos, 
                          pfuturo_ganancia, pfuturo_auc, pfuturo_canaritos,
                          ptiempo, pprob_corte, pst_parametros, 
                          parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase )

}
#------------------------------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +19500 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -500 )

#Variable Global que va a modificar la probabilidad de corte
# como multiplico, 1.0 significa que NO lo cambio
Gprobcorte_mult <-  1.0

fmetrica_ganancia  <- function( probs, clases, pclase_valor_positivo, problema )
{
 
  return(  sum(    (probs > problema$prob_corte*Gprobcorte_mult ) * 
                   ifelse( clases== pclase_valor_positivo, problema$ganancia_acierto, problema$ganancia_noacierto )   
              )
         )
}
#------------------------------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_lightgbm  <- function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------
#Esta parte del codigo es MUY  OSCURA
#En la misma funcino se estima el punto de corte optimo sobre la mitad de testing
#Y se calcula la ganancia sobre la OTRA mitad de testing
#El usar el campo  weight, es realmente una solucion MUY TURBIA
#Esta sola funcion demanda 1 hora de explicacion en el pizarron
#debe entenderse que esta fijo lo que recibe y fijo lo que devuelve

Glgbm_actual <- list( "ganancia_mejor"=0, "prob_corte"=0 )


fganancia_logistic_lightgbm   <- function(probs, clases) 
{

   vlabels   <- getinfo(clases, "label")
   vweights  <- getinfo(clases, "weight")

   tbl <- as.data.table( cbind(probs, vlabels, vweights) )
   colnames( tbl ) <-  c( "prob", "clase", "peso" )
   
   #me quedo solo con los datos de validation para estimar la mejor probabilidad de corte
   tbl <- tbl[ peso>1.0, ]
   setorder(tbl, -prob )
   tbl[ , ganancia:= ifelse( clase==1,  env$problema$ganancia_acierto, env$problema$ganancia_noacierto ) ]
   tbl[ , ganacum:= cumsum( tbl$ganancia ) ]
   pos <- which.max( tbl$ganacum )
   vprob_corte <-  tbl[ pos, prob ]
   

   gan <-2.0 * sum(   (vweights==1.0) * (probs > vprob_corte  ) * 
                      ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
                  )

   if( gan > Glgbm_actual$ganancia_mejor )
   {
     #setwd(env$directory$work)
     #fwrite( tbl, file="proba.txt", sep="\t" )
   
     Glgbm_actual$prob_corte       <<- vprob_corte
     Glgbm_actual$ganancia_mejor   <<- gan
   }

   return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}
#------------------------------------------------------------------------------

Gprob_corte_futuro <- 0.0

fganancia_logistic_lightgbm_futuro   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > Gprob_corte_futuro  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}
#------------------------------------------------------------------------------

modelo_lightgbm_actual <- function( ptrain, ptest, ptest_clase, pclase_nomcampo, pclase_valor_positivo, pproblema, pprobcorte_mult,
                                    pfeature_fraction, plearning_rate, plambda_l1, plambda_l2, pmin_gain_to_split, pmin_data_in_leaf, pmax_depth )
{
  Glgbm_actual$prob_corte       <<- 0
  Glgbm_actual$ganancia_mejor   <<- 0
  
  #La gran llamada a  LightGBM
  modelo = lgb.train( 
                      data = ptrain,  
                      objective="binary",
                      valids= list( valid=ptest),
                      eval = fganancia_logistic_lightgbm, 
                      metric = "auc" ,
                      num_leaves= env$lightgbm$num_leaves,
                      num_iterations= env$lightgbm$num_iterations_max,
                      early_stopping_rounds= env$lightgbm$early_stopping_round,
                      max_bin = env$lightgbm$max_bin,
                      boost_from_average= TRUE ,
                      subsample = env$lightgbm$subsample, 
                      feature_fraction = pfeature_fraction, 
                      learning_rate = plearning_rate,
                      min_data_in_leaf = pmin_data_in_leaf, 
                      max_depth = pmax_depth,
                      lambda_l1 = plambda_l1, lambda_l2 = plambda_l2, min_gain_to_split = pmin_gain_to_split,
                      verbosity= -1
                     )
                     
  iteracion_max      <- which.max( unlist(modelo$record_evals$valid$ganancia$eval) )
  ganancia           <- unlist(modelo$record_evals$valid$ganancia$eval)[ iteracion_max ] 
  
  auc                <- unlist(modelo$record_evals$valid$auc$eval)[ iteracion_max ] 
  
  tb_importancia     <-  as.data.table( lgb.importance(  modelo) )

  vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )

  return( list("ganancia_test"=ganancia, 
               "auc_test"=auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "num_iterations"= iteracion_max,
               "prob_corte"= Glgbm_actual$prob_corte,
               "importancia"=tb_importancia ) )
}
#------------------------------------------------------------------------------

modelo_lightgbm_futuro <- function( ptrain, ptest, ptest_clase, pclase_nomcampo, pclase_valor_positivo, pproblema, pprobcorte,
                                    pfeature_fraction, plearning_rate, plambda_l1, plambda_l2, pmin_gain_to_split, pmin_data_in_leaf, pmax_depth, pnum_iterations )
{

  #Asigno la variable global  Gprob_corte_futuro
  Gprob_corte_futuro <<- pprobcorte
  
  #La gran llamada a  LightGBM
  modelo = lgb.train( 
                      data = ptrain,  
                      objective="binary",
                      valids= list( valid=ptest),
                      eval = fganancia_logistic_lightgbm_futuro,
                      metric = "auc" ,
                      num_leaves= env$lightgbm$num_leaves,
                      num_iterations= pnum_iterations,
                      max_bin = env$lightgbm$max_bin,
                      boost_from_average= TRUE ,
                      subsample = env$lightgbm$subsample, 
                      feature_fraction = pfeature_fraction, 
                      learning_rate = plearning_rate,
                      min_data_in_leaf = pmin_data_in_leaf, 
                      max_depth = pmax_depth,
                      lambda_l1 = plambda_l1, lambda_l2 = plambda_l2, min_gain_to_split = pmin_gain_to_split,
                      verbosity= -1
                     )
                     
                     
  iteracion_max      <- pnum_iterations
  ganancia           <- unlist(modelo$record_evals$valid$ganancia$eval)[ iteracion_max ] 
  
  auc                <- unlist(modelo$record_evals$valid$auc$eval)[ iteracion_max ] 
  
  tb_importancia     <-  as.data.table( lgb.importance(  modelo) )

  vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )

  return( list("ganancia_test"=ganancia, 
               "auc_test"=auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "num_iterations"= iteracion_max,
               "importancia"=tb_importancia ) )
}
#------------------------------------------------------------------------------
x <- list( pventana=10, pfeature_fraction=0.45346964467247, plearning_rate=0.0400473344887141, plambda_l1=0.77902263274882, plambda_l2=18.5116389467148, pmin_gain_to_split=1.20450788270682, pmin_data_in_leaf=29, pmax_depth=15, pnum_iterations=400)

Gmbo  <- list( "ganancia_mejor"=0, "num_iterations"=0, "prob_corte"=0 )
glob_mfuturo_ganancia_test <- 0


modelo_lightgbm_ganancia_MBO_directo <- function( x )
{
  gc()
  t0   <-  Sys.time()

  vmes_actual_train  <- glob_mes_actual_train 
  vmes_actual_test   <- vmes_actual_train -2
  
  vmes_futuro_train  <- glob_mes_actual_train -2
  vmes_futuro_test   <- vmes_futuro_train - 2
  
  #aqui la clase es 1={BAJA+2,BAJA+2}  0={CONTINUA}
  dactual_train <-   lgb.Dataset( data  = as.matrix(dataset_grande[ (sample < env$undersampling | clase_ternaria!=env$data$clase_valor_negativo) & mes<=(vmes_actual_train + x$pventana-1)  &  mes>=vmes_actual_train & mes!=vmes_actual_test, 
                                                           !c("sample",env$data$campo_id, env$data$clase_nomcampo), 
                                                           with=FALSE]),
                                  label = dataset_grande[ (sample < env$undersampling | clase_ternaria!=env$data$clase_valor_negativo) & mes<=(vmes_actual_train + x$pventana-1)  &  mes>=vmes_actual_train & mes!=vmes_actual_test,
                                                           as.integer( get(env$data$clase_nomcampo) != env$data$clase_valor_negativo  ) ] ,
                                  free_raw_data=FALSE
                                )

  mactual  <- modelo_lightgbm_actual(
                             dactual_train,  
                             dactual_test, 
                             dactual_test_clase, 
                             env$data$clase_nomcampo, 
                             env$data$clase_valor_positivo, 
                             env$problema, 
                             pfeature_fraction= x$pfeature_fraction,  
                             plearning_rate= x$plearning_rate, 
                             plambda_l1= x$plambda_l1,
                             plambda_l2= x$plambda_l2,
                             pmin_gain_to_split= x$pmin_gain_to_split,
                             pmin_data_in_leaf= x$pmin_data_in_leaf,
                             pmax_depth= x$pmax_depth
                            )

  
  if( mactual$ganancia_test >  Gmbo$ganancia_mejor )
  {
    Gmbo$ganancia_mejor    <<- mactual$ganancia_test
    Gmbo$num_iterations    <<- mactual$num_iterations
    Gmbo$prob_corte        <<- mactual$prob_corte
  }

  if( glob_procesar_futuro )
  {
    #Aqui no hago undersampling
    #aqui la clase es 1={BAJA+2,BAJA+2}  0={CONTINUA}
    dfuturo_train <-   lgb.Dataset( data  = as.matrix(dataset_grande[ mes<=(vmes_futuro_train+x$pventana-1)  &  mes>=vmes_futuro_train & mes!=vmes_futuro_test,
                                                              !c("sample",env$data$campo_id, env$data$clase_nomcampo), 
                                                               with=FALSE]),
                                      label = dataset_grande[ mes<=(vmes_futuro_train+x$pventana-1)  &  mes>=vmes_futuro_train & mes!=vmes_futuro_test,
                                                              as.integer( get(env$data$clase_nomcampo) != env$data$clase_valor_negativo  ) ] ,
                                      free_raw_data=FALSE
                                    )

    #con un poco de algebra, partiendo de la probabilidad de corte cuando  1={BAJA+1,BAJA+2}
    #calculo cuanto tiene que dar la probabilidad de corte cuendo  1={BAJA+2}
    vmult <- (1/env$undersampling)
    vgan_mala <-  - env$problema$ganancia_noacierto
    vprob_corte_futuro <-  vgan_mala / (  vgan_mala +  vmult*vgan_mala*( 1/mactual$prob_corte -1 ) ) 
    
    
    mfuturo  <- modelo_lightgbm_futuro(
                               dfuturo_train,  
                               dfuturo_test, 
                               dfuturo_test_clase, 
                               env$data$clase_nomcampo, 
                               env$data$clase_valor_positivo, 
                               env$problema,
                               pprobcorte= vprob_corte_futuro,
                               pfeature_fraction= x$pfeature_fraction,  
                               plearning_rate= x$plearning_rate, 
                               plambda_l1= x$plambda_l1,
                               plambda_l2= x$plambda_l2,
                               pmin_gain_to_split= x$pmin_gain_to_split,
                               pmin_data_in_leaf= x$pmin_data_in_leaf,
                               pmax_depth= x$pmax_depth,
                               pnum_iterations= mactual$num_iterations                             
                              )


    glob_mfuturo_ganancia_test <<- mfuturo$ganancia_test
  
    t1   <-  Sys.time()
    tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")


    #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

    #genero el string con los parametros
    st_parametros = paste("ventana=",          x$pventana,             ", ",
                          "probcorte=",        mactual$prob_corte,     ", ",
                          "num_iterations=",   mactual$num_iterations, ", ",
                          "learning_rate=",    x$plearning_rate,       ", ",
                          "lambda_l1=",        x$plambda_l1,           ", ",
                          "lambda_l2=",        x$plambda_l2,           ", ",
                          "min_gain_to_split=",x$pmin_gain_to_split,   ", ",
                          "min_data_in_leaf=", x$pmin_data_in_leaf,    ", ",
                          "max_depth=",        x$pmax_depth,           ", ",
                          "feature_fraction=", x$pfeature_fraction,    ", ",
                          "max_bin=",          env$lightgbm$max_bin,   ", ",
                          "subsample=",        env$lightgbm$subsample,
                          sep = ""
                         )


    #escribo al archivo de salida los resultados de esta corrida
    hiperparametros_grabar( phiper= env$hiper,
                            pactual_ganancia= mactual$ganancia_test, 
                            pactual_auc= mactual$auc_test,
                            pactual_canaritos= mactual$canaritos_muertos,
                            pfuturo_ganancia= mfuturo$ganancia_test, 
                            pfuturo_auc= mfuturo$auc_test,
                            pfuturo_canaritos= mfuturo$canaritos_muertos,
                            ptiempo= tiempo_corrida,
                            pprob_corte= vprob_corte_futuro, 
                            pst_parametros= st_parametros, 
                            parchivo_actual_train= paste( tb_meses[ mes==(vmes_actual_train+x$pventana-1), foto_mes]  ,"-",tb_meses[ mes==vmes_actual_train, foto_mes]),
                            parchivo_actual_test=  tb_meses[ mes==vmes_actual_test, foto_mes],
                            parchivo_futuro_train= paste( tb_meses[ mes==vmes_futuro_train+x$pventana-1, foto_mes]  ,"-",tb_meses[ mes==vmes_futuro_train, foto_mes]),
                            parchivo_futuro_test=  tb_meses[ mes==vmes_futuro_test, foto_mes],
                            pclase="ternaria",
                            pcanaritos_muertos= mactual$canaritos_muertos
                          )
  
    #Grabo la importancia de las variables
    setwd( env$directory$work )
    fwrite(  mactual$importancia,
             file = paste0( "importancia_", env$experimento,".txt"),
             sep="\t" )
  
  }
  
  
  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

agregar_canaritos <- function( pdataset,  pcanaritos_cantidad )
{
  vcanaritos <-  paste0( "canarito", 1:pcanaritos_cantidad )

  #uso esta semilla para los canaritos
  set.seed(10219)

  pdataset[ , (vcanaritos) := 0 ]
  pdataset[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]

  #ahora hago que los canaritos sean las primeras variables del dataset
  nuevo_orden <-  c( vcanaritos, setdiff( colnames( pdataset), vcanaritos )) 
  setcolorder( pdataset, nuevo_orden )
}
#------------------------------------------------------------------------------
#pmes_cero  es el mes del futuro donde supuestamente no tengo clase 
pmes_cero <- 201904


optimizacion_bayesiana <- function( pmes_cero )
{

  glob_procesar_futuro  <<- 0
  Gmbo$ganancia_mejor   <<- 0
  Gmbo$num_iterations   <<- 0
  Gmbo$prob_corte       <<- 0
  
  
  glob_mes_actual_train     <<-  tb_meses[ foto_mes==pmes_cero, mes] + 2 + 2

  vmes_actual_train  <- glob_mes_actual_train 
  vmes_actual_test   <- vmes_actual_train - 2
  
  vmes_futuro_train  <- glob_mes_actual_train - 2
  vmes_futuro_test   <- vmes_futuro_train - 2

  
  #en estos dos datasets, que es donde mido la ganancia, se cumplen dos cosas
  # 1=BAJA+2  y  0 = {BAJA+1,CONTINUA}
  # no hago undersampling, van completos
  vazar  <- runif( nrow(dataset_grande[ mes==vmes_actual_test,]) )
  vpesos <- ifelse( vazar<0.5, 1.0, 1.00000001 )
  dactual_test  <<-   lgb.Dataset( data  = as.matrix(dataset_grande[mes==vmes_actual_test, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                  label  = dataset_grande[ mes==vmes_actual_test, as.integer( get(env$data$clase_nomcampo) == env$data$clase_valor_positivo  )],
                                  weight = vpesos,
                                  free_raw_data=FALSE
                                )

  dfuturo_test  <<-   lgb.Dataset( data  = as.matrix(dataset_grande[mes==vmes_futuro_test, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                  label = dataset_grande[ mes==vmes_futuro_test,as.integer( get(env$data$clase_nomcampo) == env$data$clase_valor_positivo  )],
                                  free_raw_data=FALSE
                                )


  hiperparametros_crear(env$hiper)


  configureMlr(show.learner.output = FALSE)

  funcion_optimizar <-  modelo_lightgbm_ganancia_MBO_directo

  #configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
  obj.fun <- makeSingleObjectiveFunction(
             name = "prueba",
             fn   = funcion_optimizar,
             par.set = makeParamSet(
               makeNumericParam("pfeature_fraction" ,  lower=0.05    , upper=   1.0),
               makeNumericParam("plearning_rate"    ,  lower=0.0     , upper=   0.3),
               makeNumericParam("plambda_l1"        ,  lower=0.0     , upper=  50.0),
               makeNumericParam("plambda_l2"        ,  lower=0.0     , upper=  50.0),
               makeNumericParam("pmin_gain_to_split",  lower=0.0     , upper=  20.0),
               makeIntegerParam("pmin_data_in_leaf" ,  lower=0L      , upper= 100L),
               makeIntegerParam("pmax_depth"        ,  lower=2L      , upper=  20L),
               makeIntegerParam("pventana"          ,  lower=1L      , upper=  10L)
              ),
            has.simple.signature = FALSE,
            global.opt.value = -1
            )


  varchivo_trabajo <-  paste0("exp_", env$experimento, "_", pmes_cero, ".RDATA")

  ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = env$mbo$saveondisk_time,  save.file.path = varchivo_trabajo)
  ctrl <-  setMBOControlTermination(ctrl, iters = env$mbo$iteraciones )
  ctrl <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = env$mbo$iteraciones)

  surr.km <-  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))


  setwd(env$directory$work)
  
  if(!file.exists(varchivo_trabajo))
  {
    #lanzo la busqueda bayesiana
    run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    #retoma el procesamiento en donde lo dejo
    run <- mboContinue( varchivo_trabajo )
  }
 
  #Ahora vuelvo a calacular todo, con la mejor corrida  run$x
  #PERO esta vez genero el modelo del futuro, lo aplico
  #y grabo la linea en hiperparametro_xxxx.txt
  glob_procesar_futuro  <<- 1
  Gmbo$ganancia_mejor   <<- 0
  Gmbo$num_iterations   <<- 0
  Gmbo$prob_corte       <<- 0
 

  modelo_lightgbm_ganancia_MBO_directo( run$x )
  
  
  
  #Si pmes_cero es 201904, entonces ya genero la salida de la materia
  if( pmes_cero== 201904 )
  {
    #Aplico el mejor modelo que encontro la  Optimizacion Bayesiana
    #A los datos de 201906, los que tengo que predecir !!!

    #Aqui no hago undersampling, voy a entrenar con TODO el dataset
    #Pero si hago que  1={BAJA+1, BAJA+2}
    #run$x$pventana  tiene la cantidad de meses optimos en los que hay que entrenar
    train_final  <-  dataset_grande[ foto_mes <= 201904  &  mes >= run$x$pventana+vmes_futuro_test-1, ]


    dtrain_final  <-   lgb.Dataset( data  = as.matrix(train_final[, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                    label = train_final[ , as.integer( get(env$data$clase_nomcampo) != env$data$clase_valor_negativo  )], 
                                    free_raw_data=FALSE 
                                  )
 
    mfinal = lgb.train( 
                        data= dtrain_final,  
                        objective= "binary",
                        eval= fganancia_logistic_lightgbm, 
                        num_leaves= env$lightgbm$num_leaves,
                        num_iterations= Gmbo$num_iterations,
                        max_bin= env$lightgbm$max_bin,
                        boost_from_average= TRUE ,
                        subsample= env$lightgbm$subsample, 
                        feature_fraction= run$x$pfeature_fraction, 
                        learning_rate= run$x$plearning_rate,
                        min_data_in_leaf= run$x$pmin_data_in_leaf, 
                        max_depth= run$x$pmax_depth,
                        lambda_l1= run$x$plambda_l1, 
                        lambda_l2= run$x$plambda_l2, 
                        min_gain_to_split= run$x$pmin_gain_to_split,
                      ) 


    #Los datos de 201906,  que no tienen clase
    test_final <- dataset_grande[  foto_mes== 201906, ]

    #aplico el modelo a datos nuevos
    testing_prediccion  <- predict( mfinal,  
                                    as.matrix(test_final[,!c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE ] ) )

    #pego los numero_de_cliente  con la probabilidad
    tb_prediccion <-  as.data.table( cbind(   test_final[ , get(env$data$campo_id) ] , testing_prediccion ) )
    colnames( tb_prediccion ) <-  c( "ID", "prob" ) 

    #ordeno por probabilidad descendente
    setorder( tb_prediccion, -prob)

    #Grabo todas las probabilidades
    setwd(env$directory$work)
    fwrite(  tb_prediccion,
             file =  paste0( "salida_", env$experimento, "_completo.txt" ),
             sep="\t" 
          )         

   #Genero la salida de la materia 
   #Entrene sobre todos los datos, la probabilidad de corte es 0.025 * run$x$pprobcorte_mult
   vmult <- (1/env$undersampling)
   vgan_mala <-  - env$problema$ganancia_noacierto
   vprob_corte_futuro <-  vgan_mala / (  vgan_mala +  vmult*vgan_mala*( 1/Gmbo$prob_corte -1 ) ) 
 
   fwrite(  tb_prediccion[ prob > vprob_corte_futuro, "ID"],
            file =paste0( "salida_", env$experimento, "_entregamateria.txt" ),
            col.names= FALSE,
            sep="\t" 
         )         
  }
  
  return(  list("mes_cero"=pmes_cero,  "metrica1_futuro"= glob_mfuturo_ganancia_test) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

#cargo los archivos de entrada
setwd( env$directory$datasets)
if( env$data$archivo_grande %like% "gz" ) 
{
  dataset_grande   <- fread(env$data$archivo_grande)
} else {
  dataset_grande   <- fread(cmd=paste("cat", env$data$archivo_grande))
}

#Borro campos
if( length(env$data$campos_a_borrar)>0 )  dataset_grande[ ,  (env$data$campos_a_borrar) := NULL    ] 


#agrego variable para el undersampling
set.seed(410551)
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]

#agrego las variables canarito
agregar_canaritos( dataset_grande, 100 )

#agrego la columna  mes_actual
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tb_meses ) <- c( "mes", "foto_mes" )

dataset_grande[  tb_meses,  on="foto_mes",  mes:= i.mes ]

#creo el archivo de los hiperparametros
hiperparametros_crear(env$hiper)

meses_a_procesar  <-   tb_meses[  foto_mes>=201806  & foto_mes<=201904, foto_mes ] 
if( env$procesar_solo_201904 )  meses_a_procesar= c( 201904 )



#Atencion, aqui empieza la corrida pesada
#corro una optimizacion bayesiana para cada uno de los meses 201904, 201903, 201902,  ...
res <- lapply( meses_a_procesar, optimizacion_bayesiana )


#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

