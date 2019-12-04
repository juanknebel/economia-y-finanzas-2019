#Este programa hace ONCE optimizaciones bayesianas
#Se utiliza el metodo de Walk Forward Validation
#XGBoost


#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("ROCR")
library("xgboost")

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


env$experimento          <-  311
env$procesar_solo_201904 <-  FALSE

env$undersampling        <-  1.0


#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c("ttarjeta_visa_debitos_automaticos", 
                                "ttarjeta_visa_debitos_automaticos__min",
                                "ttarjeta_visa_debitos_automaticos__max",
                                "ttarjeta_visa_debitos_automaticos__tend",
                                "chomebanking_transacciones",
                                "chomebanking_transacciones__min",
                                "chomebanking_transacciones__max",
                                "chomebanking_transacciones__tend"
                               )
                               
data$archivo_grande       <-  "paquete_premium_exthist_modelitosxval.txt.gz"
env$data <- data


#Parametros  mlrMBO   Optimizacion Bayesiana
mbo <- list()
mbo$iteraciones       <-   80   # cuantas iteraciones hace la Optimizacion Bayesiana
mbo$saveondisk_time   <-  600   # cada 600 segundos guarda a disco cuanto avanzo
env$mbo <- mbo

#sobre el funcionamiento de programa
#en el XGboost
xgb <- list()
xgb$semilla               <- 102191
xgb$max_bin               <-     31
xgb$subsample             <-      1.0
xgb$nround_max            <-   1000
xgb$nround_early_stopping <-     50
env$xgboost <- xgb



#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory_work
hiper$clase_tipo     <-  "binaria1"
hiper$programa       <-  "xgboost_directo_wfv.r"
hiper$algoritmo      <-  "xgboost"
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

glob_mes_cero <- 201904

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

fmetrica_auc_xgboost  = function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

fganancia_logistic_xgboost_ACTUAL   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > env$problema$prob_corte  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, gan ) )  )
}
#------------------------------------------------------------------------------
#aqui la probabilidad de corte es 0.025 , porque NO HAGO  undersampling

fganancia_logistic_xgboost_FUTURO   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > 0.025  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, gan ) )  )
}
#------------------------------------------------------------------------------
#Aqui se hace  EARLY STOPPING
glob_ganancia_mejor <- 0
glob_num_iterations_mejor   <- 0

modelo_xgboost_actual <- function( ptrain, ptest, xx )
{
  #La gran llamada a  XGBoost
  modelo = xgb.train( 
                      data= ptrain,  
                      missing= NA,
                      objective= "binary:logistic",
                      watchlist= list( test=ptest ),
                      feval= fganancia_logistic_xgboost_ACTUAL,  maximize= TRUE, 
                      nround= env$xgboost$nround_max,
                      early_stopping_rounds= env$xgboost$nround_early_stopping,
                      tree_method= "hist",
                      max_bin= env$xgboost$max_bin,
                      base_score= mean( getinfo(ptrain, "label") ) ,
                      subsample= env$xgboost$subsample, 
                      colsample_bytree= xx$pcolsample_bytree, 
                      eta= xx$peta,
                      min_child_weight= xx$pmin_child_weight, 
                      max_depth= xx$pmax_depth,
                      alpha= xx$palpha, 
                      lambda= xx$plambda, 
                      gamma= xx$pgamma
                     )

  iteracion_optima   <- which.max( unlist(modelo$evaluation_log[ , test_ganancia]) )
  ganancia           <- unlist( modelo$evaluation_log[ , test_ganancia] )[ iteracion_optima ] 
  
  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest)

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( testing_prediccion,  getinfo(ptest, "label") ) 

  vcanaritos_muertos <-  NA

  if( ganancia >=  glob_ganancia_mejor )
  {
    glob_ganancia_mejor  <<- ganancia
    glob_nround_mejor    <<- iteracion_optima

    cat( "ganancia_mejor:", glob_ganancia_mejor, " ", glob_nround_mejor, "\n")

    #grabo la importancia aqui por si quiero abortar la corrida y reducir 
    tb_importancia <-  as.data.table( xgb.importance( dimnames(ptrain)[[2]], modelo, trees=0:(iteracion_optima-1) ) )
    setwd( env$directory$work )
    fwrite( tb_importancia,
            file = paste0( "importancia_", env$experimento, "_", glob_mes_cero, ".txt"),
            sep="\t" )

    vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )
  }
  cat( "ganancia_mejor:", glob_ganancia_mejor, "\n")

  return( list("ganancia_test"= ganancia, 
               "auc_test"= auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "nround"= iteracion_optima ) )
}
#------------------------------------------------------------------------------
#Aqui NO se hace  early stopping, en  actual ya se calculo el nround
glob_mfuturo_ganancia_test <- 0

modelo_xgboost_futuro <- function( ptrain, ptest, zz )
{

  #La gran llamada a  XGBoost
  modelo = xgb.train( 
                      data= ptrain,  
                      missing= NA,
                      objective= "binary:logistic",
                      watchlist= list( test=ptest ),
                      feval= fganancia_logistic_xgboost_FUTURO,  maximize= TRUE, 
                      nround= zz$pnround,
                      tree_method= "hist",
                      max_bin= env$xgboost$max_bin,
                      base_score= mean( getinfo(ptrain, "label") ) ,
                      subsample= env$xgboost$subsample, 
                      colsample_bytree= zz$pcolsample_bytree, 
                      eta= zz$peta,
                      min_child_weight= zz$pmin_child_weight, 
                      max_depth= zz$pmax_depth,
                      alpha= zz$palpha, 
                      lambda= zz$plambda,
                      gamma= zz$pgamma
                     )
                     
                     
  iteracion_optima   <- zz$pnround
  ganancia           <- unlist( modelo$evaluation_log[ , test_ganancia] )[ iteracion_optima ] 
  
  glob_mfuturo_ganancia_test <<- ganancia
  
  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest)

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( testing_prediccion,  getinfo(ptest, "label") ) 

  tb_importancia <-  as.data.table( xgb.importance( dimnames(ptrain)[[2]], modelo, trees=0:(iteracion_optima-1) ) )

  vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )

  return( list("ganancia_test"= ganancia, 
               "auc_test"= auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "nround"= iteracion_optima,
               "importancia"= tb_importancia ) )
}
#------------------------------------------------------------------------------

x <- list( pventana=10, pcolsample_bytree=0.6, peta=0.04, palpha=0.0, plambda=0.0, pgamma=0.0, pmin_child_weight=1, pmax_depth=6)

glob_procesar_futuro   <-  1


modelo_xgboost_ganancia_MBO_directo <- function( x )
{
  gc()
  t0   <-  Sys.time()

  vmes_actual_train  <- glob_mes_actual_train 
  vmes_actual_test   <- vmes_actual_train -2
  
  vmes_futuro_train  <- glob_mes_actual_train -2
  vmes_futuro_test   <- vmes_futuro_train - 2


  #marco en el campo  train  los registros que participan del entrenamiento
  dataset_grande[  , train:= 0 ]
  dataset_grande[  (sample < env$undersampling | clase01== 1) & mes<=(vmes_actual_train+x$pventana-1)  &  mes>=vmes_actual_train & mes!=vmes_actual_test ,
                  train:=1 ]

  dactual_train <-   xgb.DMatrix( data  = data.matrix( dataset_grande[ train== 1, 
                                                                       !c("sample","train", "clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                                       with=FALSE]),
                                  label = dataset_grande[ train== 1,
                                                          clase01 ] ,
                                  missing=NA 
                                )

  xx <- x
  xx$pventana <- NULL  #no hace falta

  mactual  <- modelo_xgboost_actual( dactual_train,  
                                     dactual_test, 
                                     xx )


  if( glob_procesar_futuro )
  {
    #En el futuro NO se hace UNDERSAMPLING
    dataset_grande[  , train:= 0 ]
    dataset_grande[  mes<=(vmes_futuro_train+x$pventana-1)  &  mes>= vmes_futuro_train & mes!= vmes_futuro_test ,
                     train:=1 ]

    dfuturo_train <-   xgb.DMatrix( data= data.matrix( dataset_grande[ train== 1,
                                                                       !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                                       with=FALSE]),
                                    label = dataset_grande[ train== 1,
                                                            clase01 ] ,
                                    missing=NA 
                                  )

    zz <- xx
    zz$pnround <- mactual$nround

    mfuturo  <- modelo_xgboost_futuro( dfuturo_train,
                                       dfuturo_test, 
                                       zz  )

    t1   <-  Sys.time()
    tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



    #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

    #genero el string con los parametros
    st_parametros = paste("ventana=",          x$pventana,          ", ",
                          "nround=",           mactual$nround,      ", ",
                          "eta=",              x$peta,              ", ",
                          "alpha=",            x$palpha,            ", ",
                          "lambda=",           x$plambda,           ", ",
                          "gamma=",            x$pgamma,            ", ",
                          "min_child_weight=", x$pmin_child_weight, ", ",
                          "max_depth=",        x$pmax_depth,        ", ",
                          "colsample_bytree=", x$pcolsample_bytree, ", ",
                          "max_bin=",          env$xgboost$max_bin, ", ",
                          "subsample=",        env$xgboost$subsample,
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
                            pprob_corte= env$problema$prob_corte, 
                            pst_parametros= st_parametros, 
                            parchivo_actual_train= env$data$mes_actual_train,
                            parchivo_actual_test=  env$data$mes_actual_test,
                            parchivo_futuro_train= env$data$mes_futuro_train,
                            parchivo_futuro_test=  env$data$mes_futuro_test,
                            pclase="ternaria",
                            pcanaritos_muertos= mactual$canaritos_muertos
                          )

  }
  
  return(  - mactual$ganancia_test )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
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

optimizacion_bayesiana <- function( pmes_cero )
{

  glob_mes_cero             <<- pmes_cero
  glob_procesar_futuro      <<- 0
  glob_ganancia_mejor       <<- 0
  glob_num_iterations_mejor <<- 0
  
  glob_mes_actual_train     <<-  tb_meses[ foto_mes==pmes_cero, mes] + 2 + 2

  vmes_actual_train  <- glob_mes_actual_train 
  vmes_actual_test   <- vmes_actual_train - 2
  
  vmes_futuro_train  <- glob_mes_actual_train - 2
  vmes_futuro_test   <- vmes_futuro_train - 2

  dactual_test  <<-   xgb.DMatrix( data= data.matrix(dataset_grande[mes==vmes_actual_test, !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                   label= dataset_grande[ mes==vmes_actual_test, clase01],
                                   missing= NA
                                 )
  
  
  dfuturo_test  <<-   xgb.DMatrix( data= data.matrix(dataset_grande[mes==vmes_futuro_test, !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                   label= dataset_grande[ mes==vmes_futuro_test, clase01],
                                   missing= NA
                                 )


  hiperparametros_crear(env$hiper)


  configureMlr(show.learner.output = FALSE)

  funcion_optimizar <-  modelo_xgboost_ganancia_MBO_directo

  #configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
  obj.fun <- makeSingleObjectiveFunction(
                name = "prueba",
                fn   = funcion_optimizar,
                par.set = makeParamSet(
                    makeNumericParam("pcolsample_bytree" ,  lower=0.2     , upper=   1.0),
                    makeNumericParam("peta"              ,  lower=0.01    , upper=   0.3),
                    makeNumericParam("palpha"            ,  lower=0.0     , upper=  50.0),
                    makeNumericParam("plambda"           ,  lower=0.0     , upper=  50.0),
                    makeNumericParam("pgamma"            ,  lower=0.0     , upper=  20.0),
                    makeNumericParam("pmin_child_weight" ,  lower=1.0     , upper= 100.0),
                    makeIntegerParam("pmax_depth"        ,  lower=6L      , upper=  16L),
                    makeIntegerParam("pventana"          ,  lower=5L      , upper=  12L)
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
 
  #En este punto ya termino la optimizacion bayesiana
  #Ahora imprimo el futuro pero SOLO para la mejor corrida
  glob_procesar_futuro      <<- 1
  glob_mes_cero             <<- pmes_cero
  modelo_xgboost_ganancia_MBO_directo( run$x )
  
  
  vmes_futuro       <-  tb_meses[ foto_mes==pmes_cero, mes] -2
  vfoto_mes_futuro  <-  tb_meses[ mes==vmes_futuro, foto_mes]

  

  if(  vmes_futuro >= 1 )
  {
    #Aqui no hago undersampling, voy a entrenar con TODO el dataset
    #run$x$pventana  tiene la cantidad de meses optimos en los que hay que entrenar
    vmes_entrenar <-  tb_meses[ foto_mes==pmes_cero, mes] 
    dataset_grande[ , train:= 0 ]
    dataset_grande[  mes >= vmes_entrenar  &  mes <= run$x$pventana+ vmes_entrenar-1,
                     train:=  1 ]
                   
    dtrain_final  <-   xgb.DMatrix( data= data.matrix(dataset_grande[ train==1 , !c("sample","clase01","train", env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                    label= dataset_grande[ train==1 , clase01 ], 
                                    missing= NA 
                                  )


    mfinal = xgb.train( 
                        data= dtrain_final,  
                        missing= NA,
                        objective="binary:logistic",
                        feval= fganancia_logistic_xgboost_FUTURO,  maximize= TRUE, 
                        nround= glob_nround_mejor,
                        tree_method= "hist",
                        max_bin= env$xgboost$max_bin,
                        base_score= mean( getinfo(dtrain_final, "label") ) ,
                        subsample= env$xgboost$subsample, 
                        colsample_bytree= run$x$pcolsample_bytree, 
                        eta= run$x$peta,
                        min_child_weight= run$x$pmin_child_weight, 
                        max_depth= run$x$pmax_depth,
                        alpha= run$x$palpha, 
                        lambda= run$x$plambda, 
                        gamma= run$x$pgamma,
                      )


    #Los datos a donde voy a a plicar el modelo
    test_final <- dataset_grande[  mes== vmes_futuro, ]

    #aplico el modelo a datos nuevos
    testing_prediccion  <- predict( mfinal,  
                                    as.matrix(test_final[,!c("sample","clase01","train", env$data$campo_id, env$data$clase_nomcampo), with=FALSE ] ) )

    #pego los numero_de_cliente  con la probabilidad
    tb_prediccion <-  as.data.table( cbind(   test_final[ , get(env$data$campo_id) ] , testing_prediccion ) )
    colnames( tb_prediccion ) <-  c( "ID", "prob" ) 

    #ordeno por probabilidad descendente
    setorder( tb_prediccion, -prob)

    #Grabo todas las probabilidades
    setwd(env$directory$work)
    fwrite(  tb_prediccion,
             file =  paste0( "salida_", env$experimento, "_", vfoto_mes_futuro, "_completo.txt" ),
             sep="\t" 
          )

    #Entrene sobre todos los datos, la probabilidad de corte es 0.025
    fwrite(  tb_prediccion[ prob>0.025, "ID"],
             file =paste0( "salida_", env$experimento, "_", vfoto_mes_futuro, "_entregamateria.txt" ),
             col.names= FALSE,
             sep="\t"
          )
   }
  
  return(  list("mes_cero"=pmes_cero,  "metrica1_futuro"= glob_mfuturo_ganancia_test) )
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
vmeses   <-  abs(sort(-unique( dataset_grande$foto_mes )))
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tb_meses ) <- c( "mes", "foto_mes" )
dataset_grande[  tb_meses,  on="foto_mes",  mes:= i.mes ]


#creo el archivo de los hiperparametros
hiperparametros_crear(env$hiper)

meses_a_procesar  <-   tb_meses[  foto_mes>=201806  & foto_mes<=201904, foto_mes ] 
if( env$procesar_solo_201904 )  meses_a_procesar= c( 201904 )

#ordeno para que genere mas rapido los datasets
dataset_grande[ , train:=0 ]
dataset_grande[ sample<= env$undersampling | clase01==1, train:=1 ]

setorder( dataset_grande, train, foto_mes, numero_de_cliente )

#corro una optimizacion bayesiana para cada uno de los meses 201904, 201903, 201902,  ...
res <- lapply( meses_a_procesar, optimizacion_bayesiana )


#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

