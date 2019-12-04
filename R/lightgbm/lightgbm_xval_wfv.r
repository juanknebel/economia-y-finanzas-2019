#Este programa hace ONCE optimizaciones bayesianas
#Usa  Cross Validation


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


env$experimento          <-  88102
env$procesar_solo_201904 <-  FALSE

env$undersampling        <-  0.1


#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c()
data$archivo_grande       <-  "paquete_premium_hist.txt.gz"
env$data <- data


#Parametros  mlrMBO   Optimizacion Bayesiana
mbo <- list()
mbo$iteraciones       <-  100   # cuantas iteraciones hace la Optimicacion Bayesiana
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
lgb$folds                 <-      3
env$lightgbm <- lgb



#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory_work
hiper$clase_tipo     <-  "binaria1"
hiper$programa       <-  "lightgbm_xval_wfv.r"
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
problema$prob_corte           <-   -problema$ganancia_noacierto*(1/env$undersampling)/( problema$ganancia_acierto - problema$ganancia_noacierto*(1/env$undersampling) )


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

fmetrica_ganancia  <- function( probs, clases, pclase_valor_positivo, problema )
{
 
  return(  sum(    (probs > problema$prob_corte  ) * 
                   ifelse( clases== pclase_valor_positivo, problema$ganancia_acierto, problema$ganancia_noacierto )   
              )
         )
}
#------------------------------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_lightgbm  = function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

fganancia_logistic_lightgbm   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > env$problema$prob_corte  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}
#------------------------------------------------------------------------------

fganancia_logistic_lightgbm_futuro   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > 0.025  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}
#------------------------------------------------------------------------------
#este es el unico lugar donde se hace cross validation

modelo_lightgbm_actual_xval <- function( ptrain,  xx )
{

  #La gran llamada a  LightGBM
  modelo = lgb.cv( 
                  data= ptrain,  
                  objective= "binary",
                  nfold= env$lightgbm$folds,
                  stratified= TRUE,
                  eval= fganancia_logistic_lightgbm, 
                  metric= "auc" ,
                  num_leaves= env$lightgbm$num_leaves,
                  num_iterations= env$lightgbm$num_iterations_max,
                  early_stopping_rounds= env$lightgbm$early_stopping_round,
                  max_bin= env$lightgbm$max_bin,
                  boost_from_average= TRUE ,
                  subsample= env$lightgbm$subsample, 
                  feature_fraction= xx$pfeature_fraction, 
                  learning_rate= xx$plearning_rate,
                  min_data_in_leaf= xx$pmin_data_in_leaf, 
                  max_depth = xx$pmax_depth,
                  lambda_l1= xx$plambda_l1, 
                  lambda_l2= xx$plambda_l2, 
                  min_gain_to_split= xx$pmin_gain_to_split,
                  verbose= -1
                 )
                     
  iteracion_max      <- modelo$best_iter
  ganancia           <- unlist(modelo$record_evals$valid$ganancia$eval)[ iteracion_max ] 
  
  auc                <- unlist(modelo$record_evals$valid$auc$eval)[ iteracion_max ] 

  #tengo que ajustar la ganancia a la ventana y al cross validation
  ganancia_pseudonormalizada <-  (env$lightgbm$folds * ganancia) /  sum(getinfo(ptrain, "label"))
  #ganancia_pseudonormalizada <-  (env$lightgbm$folds * ganancia)

  return( list("ganancia_test"=ganancia_pseudonormalizada, 
               "auc_test"=auc, 
               "num_iterations"= iteracion_max))

}
#------------------------------------------------------------------------------

modelo_lightgbm_futuro <- function( ptrain, ptest, zz )                                  
{

  #La gran llamada a  LightGBM
  modelo = lgb.train( 
                      data= ptrain,  
                      objective= "binary",
                      valids= list( valid=ptest),
                      eval= fganancia_logistic_lightgbm_futuro,
                      metric= "auc" ,
                      num_leaves= env$lightgbm$num_leaves,
                      num_iterations= zz$pnum_iterations,
                      max_bin= env$lightgbm$max_bin,
                      boost_from_average= TRUE ,
                      subsampl = env$lightgbm$subsample, 
                      feature_fraction= zz$pfeature_fraction, 
                      learning_rate= zz$plearning_rate,
                      min_data_in_leaf= zz$pmin_data_in_leaf, 
                      max_depth= zz$pmax_depth,
                      lambda_l1= zz$plambda_l1, 
                      lambda_l2= zz$plambda_l2, 
                      min_gain_to_split= zz$pmin_gain_to_split,
                      verbose= -1
                     )
                     
                     
  iteracion_max      <- zz$pnum_iterations
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
x <- list( pventana=10, pfeature_fraction=0.7, plearning_rate=0.1, plambda_l1=0.1, plambda_l2=3.0, pmin_gain_to_split=5, pmin_data_in_leaf=10, pmax_depth=20, pnum_iterations=100)
x <- list( pventana=10, pfeature_fraction=0.34, plearning_rate=0.074, plambda_l1=0.72, plambda_l2=26.3, pmin_gain_to_split=4.7, pmin_data_in_leaf=2, pmax_depth=10, pnum_iterations=166)

glob_procesar_futuro   <-  0
glob_mes_cero          <- 201904
glob_mes_actual_train  <-  5
glob_ganancia_mejor <- 0
glob_num_iterations_mejor   <- 0
glob_mfuturo_ganancia_test <- 0

modelo_lightgbm_ganancia_MBO_xval <- function( x )
{
  gc()
  t0   <-  Sys.time()

  vmes_actual_train  <- glob_mes_actual_train 


  vmes_futuro_train  <- glob_mes_actual_train
  vmes_futuro_test   <- glob_mes_actual_train - 2

  dataset_grande[  , train:= 0L ]
  dataset_grande[ (sample < env$undersampling | clase01==1) & mes<=(vmes_actual_train + x$pventana-1)  &  mes>=vmes_actual_train,
                  train:= 1L ]

  dactual_train <-   lgb.Dataset( data  = as.matrix(dataset_grande[ train==1, 
                                                                    !c("sample","train","clase01",env$data$campo_id, env$data$clase_nomcampo), 
                                                                    with=FALSE]),
                                  label = dataset_grande[ train==1, clase01 ],
                                  free_raw_data=FALSE
                                )

  xx <- x
  xx$pventana <- NULL
  mactual  <- modelo_lightgbm_actual_xval( dactual_train, xx )


  if( glob_procesar_futuro )
  {
    #Aqui no hago undersampling
    dataset_grande[  , train:= 0L ]
    dataset_grande[ mes<=(vmes_futuro_train + x$pventana-1)  &  mes>=vmes_futuro_train,
                    train:= 1L ]
                    
    dfuturo_train <-   lgb.Dataset( data  = as.matrix(dataset_grande[ train==1,
                                                                      !c("sample","train", "clase01",env$data$campo_id, env$data$clase_nomcampo), 
                                                                      with=FALSE]),
                                    label = dataset_grande[ train==1, clase01 ],
                                    free_raw_data=FALSE
                                  )

    zz <- x
    zz$pventana <- NULL
    zz$pnum_iterations <- mactual$num_iterations

    mfuturo  <- modelo_lightgbm_futuro( dfuturo_train, dfuturo_test, zz )
  
    t1   <-  Sys.time()
    tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")

    glob_mfuturo_ganancia_test  <<-  mfuturo$ganancia_test

    #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

    #genero el string con los parametros
    st_parametros = paste("ventana=", x$pventana,                      ", ",
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
                            pactual_canaritos= -1,
                            pfuturo_ganancia= mfuturo$ganancia_test, 
                            pfuturo_auc= mfuturo$auc_test,
                            pfuturo_canaritos= mfuturo$canaritos_muertos,
                            ptiempo= tiempo_corrida,
                            pprob_corte= env$problema$prob_corte, 
                            pst_parametros= st_parametros, 
                            parchivo_actual_train= paste( tb_meses[ mes==(vmes_actual_train+x$pventana-1), foto_mes]  ,"-",tb_meses[ mes==vmes_actual_train, foto_mes]),
                            parchivo_actual_test=  -1,
                            parchivo_futuro_train= paste( tb_meses[ mes==vmes_futuro_train+x$pventana-1, foto_mes]  ,"-",tb_meses[ mes==vmes_futuro_train, foto_mes]),
                            parchivo_futuro_test=  tb_meses[ mes==vmes_futuro_test, foto_mes],
                            pclase="binaria",
                            pcanaritos_muertos= mactual$canaritos_muertos
                          )
  
    #Grabo la importancia de las variables
    if( mactual$ganancia_test >  glob_ganancia_mejor )
    {
      glob_ganancia_mejor        <<- mactual$ganancia_test
      glob_num_iterations_mejor  <<- mactual$num_iterations
    
      setwd( env$directory$work )
      fwrite(  mfuturo$importancia,
               file = paste0( "importancia_", env$experimento,"_", glob_mes_cero,".txt"),
               sep="\t" )
    }

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

optimizacion_bayesiana_xval <- function( pmes_cero )
{

  glob_procesar_futuro      <<- 0
  glob_ganancia_mejor       <<- 0
  glob_num_iterations_mejor <<- 0
  
  
  vmes_futuro_test   <- tb_meses[ foto_mes==pmes_cero, mes]
  vmes_futuro_train  <- vmes_futuro_test + 2

  vmes_actual_train      <- vmes_futuro_train 
  glob_mes_actual_train  <- vmes_futuro_train 


  dfuturo_test  <<-   lgb.Dataset( data  = as.matrix(dataset_grande[mes==vmes_futuro_test, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                  label = dataset_grande[ mes==vmes_futuro_test, clase01 ],
                                  free_raw_data=FALSE
                                )


  hiperparametros_crear(env$hiper)


  configureMlr(show.learner.output = FALSE)

  funcion_optimizar <-  modelo_lightgbm_ganancia_MBO_xval

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
 
  #Ahora imprimo el futuro pero SOLO para la mejor corrida
  glob_procesar_futuro      <<- 1
  glob_mes_cero             <<- pmes_cero
  modelo_lightgbm_ganancia_MBO_xval( run$x )
  
  #Si pmes_cero es 201904, entonces ya genero la salida de la materia
  if( pmes_cero== 201904 )
  {
    #Aplico el mejor modelo que encontro la  Optimizacion Bayesiana
    #A los datos de 201906, los que tengo que predecir !!!

    #Aqui no hago undersampling, voy a entrenar con TODO el dataset
    #run$x$pventana  tiene la cantidad de meses optimos en los que hay que entrenar
    train_final  <-  dataset_grande[ foto_mes <= 201904  &  mes >= run$x$pventana+vmes_futuro_test-1, ]


    dtrain_final  <-   lgb.Dataset( data  = as.matrix(train_final[, !c("sample","clase01", env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                    label = train_final[ , clase01 ], 
                                    free_raw_data=FALSE 
                                  )


    mfinal = lgb.train( 
                        data= dtrain_final,  
                        objective="binary",
                        eval= fganancia_logistic_lightgbm, 
                        num_leaves= env$lightgbm$num_leaves,
                        num_iterations= glob_num_iterations_mejor,
                        max_bin= env$lightgbm$max_bin,
                        boost_from_average= TRUE ,
                        subsample= env$lightgbm$subsample, 
                        feature_fraction= run$x$pfeature_fraction, 
                        learning_rate= run$x$plearning_rate,
                        min_data_in_leaf= run$x$pmin_data_in_leaf, 
                        max_depth= run$x$pmax_depth,
                        lambda_l1= run$x$plambda_l1, 
                        lambda_l2= run$x$plambda_l2, 
                        min_gain_to_split= run$x$pmin_gain_to_split
                      ) 


    #Los datos de 201906,  que no tienen clase
    test_final <- dataset_grande[  foto_mes== 201906, ]

    #aplico el modelo a datos nuevos
    testing_prediccion  <- predict( mfinal,  
                                    as.matrix(test_final[,!c("sample","clase01",env$data$campo_id, env$data$clase_nomcampo), with=FALSE ] ) )

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
   #Entrene sobre todos los datos, la probabilidad de corte es 0.025
   fwrite(  tb_prediccion[ prob>0.025, "ID"],
            file =paste0( "salida_", env$experimento, "_entregamateria.txt" ),
            col.names= FALSE,
            sep="\t" 
         )         
  }
  
  return(  list("mes_vero"=pmes_cero,  "metrica1_futuro"= glob_mfuturo_ganancia_test) )
}
#------------------------------------------------------------------------------


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

#dejo la clase en {0,1}  clase  binaria1
dataset_grande[, clase01 := as.numeric( get(env$data$clase_nomcampo) == env$data$clase_valor_positivo  )] 

#agrego variable para el undersampling
set.seed(410551)
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]

#agrego las variables canarito
agregar_canaritos( dataset_grande, 34*4 )

#agrego la columna  mes_actual
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tb_meses ) <- c( "mes", "foto_mes" )

dataset_grande[  tb_meses,  on="foto_mes",  mes:= i.mes ]

#creo el archivo de los hiperparametros
hiperparametros_crear(env$hiper)

meses_a_procesar  <-   tb_meses[  foto_mes>=201806  & foto_mes<=201904, foto_mes ] 
if( env$procesar_solo_201904 )  meses_a_procesar= c( 201904 )

#ordeno el dataset
setorderv( dataset_grande, c( env$data$campo_foto, env$data$campo_id ) )

#corro una optimizacion bayesiana para cada uno de los meses 201904, 201903, 201902,  ...
res <- lapply( meses_a_procesar, optimizacion_bayesiana_xval )


#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

