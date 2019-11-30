#modelo XGBoost
#la cantidad optima de arboles de XGBoost se estima con Early Stopping


#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("ROCR")
library("Matrix")
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


env$experimento          <-  3501

env$undersampling        <-  0.10

#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c()
data$archivo_grande       <-  "paquete_premium_dias.txt"
data$mes_futuro_test      <-  1 + 2
data$mes_futuro_train     <-  1 + 2 + 2
data$mes_actual_test      <-  1 + 2 + 2
data$mes_actual_train     <-  1 + 2 + 2 + 2
data$mes_primero          <-  201712
data$mes_ultimo_conclase  <-  201904
data$mes_ultimo           <-  201906
env$data <- data


#Parametros  mlrMBO
mbo <- list()
mbo$iteraciones       <-  200
mbo$saveondisk_time   <-  600   # cada 600 segundos guarda a disco cuanto avanzo
mbo$archivo_trabajo   <-  paste("exp_", env$experimento, ".RDATA", sep="") 
env$mbo <- mbo

#sobre el funcionamiento de programa
#en el XGboost
xgb <- list()
xgb$semilla               <- 102191
xgb$max_bin               <-     31
xgb$subsample             <-      1.0
xgb$nround_max            <-   1000
xgb$nround_early_stopping <-     60
env$xgboost <- xgb



#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_imagen    <-  paste( "exp_", env$experimento, ".jpg", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory_work
hiper$clase_tipo     <-  "binaria1"
hiper$programa       <-  "xgboost_tune_MBO_meses_undersampling.r"
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
#------------------------------------------------------
#dibujo la evolucion de la metrica

hiperparametros_refrescar  <- function( phiper )
{
  campo_metrica  <- "metrica1_actual"
  campo_metrica2 <- "metrica2_actual"
  campo_metrica_futuro  <- "metrica1_futuro"
  campo_tiempo   <- "tiempo"

  #leo el archivo de salida, que tiene la info para graficar la evolucion
  setwd(phiper$directory)
  salida <-  fread(phiper$arch_local, header=TRUE, sep=phiper$separador)
  salida <-  salida[ experimento== phiper$experimento, ] 

  if(nrow(salida) >= 1)
  {

    #una hora tiene 3600 segundos, un dia tiene 24 horas
    tiempoacum  <- cumsum( salida[, get(campo_tiempo)  ] ) /(3600*24)
    metricamax  <- cummax( salida[, get(campo_metrica)  ])

  
    #dibujo la curva
    setwd(directory$work)
    jpeg(file = phiper$arch_imagen,  width = 10, height = 8, units = 'in', res = 300)

    #dos graficos en una misma salida
    par(mfrow=c(2,1))

    tituvar  <-  paste("(iter=", nrow(salida), " max=", max(salida[ , get(campo_metrica) ]),  ")")
    plot(tiempoacum, 
          metricamax, 
          type="n",
          main=paste("Evolucion Metrica", tituvar), 
          xlab="tiempo en DIAS ", 
          ylab="Metrica ", 
          pch=19)

    lines(tiempoacum, metricamax, type="l" , col="red", lwd=2)
    lines(tiempoacum, salida[ , get(campo_metrica) ], type="l" , col="blue")

    salida1 <- salida
    vcampos1 <-  c( campo_metrica )
    setorderv( salida1, vcampos1, order=-1 )
    salida1[  , idtemp :=  1:.N ]

    x1      <-  salida1[, idtemp ]
    y1      <-  salida1[ , get(campo_metrica_futuro) ]
  
    salida2 <-  salida
    vcampos2 <-  c( campo_metrica2 )
    setorderv( salida2, vcampos2, order=-1 )
    salida2[  , idtemp :=  1:.N ]



    x2      <-  salida2[, idtemp ]
    y2      <-  salida2[ , get(campo_metrica_futuro) ]


    tituvar  <-  paste( "azul gan, verde auc", nrow(salida) )
    plot( x1, 
          y1, 
          type="n",
          main=paste("Evolucion Metrica", tituvar), 
          xlab="iteracion", 
          ylab="metrica ", 
          ylim= c( pmin( min(y1),  min(y2) ), 
                   pmax( max(y1),  max(y2) ) ),
          pch=19)

    lines(x1, y1, type="l" , col="blue", lwd=1)
    lines(x2, y2, type="l" , col="green3")

    dev.off()

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

fganancia_logistic_xgboost   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > env$problema$prob_corte  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, gan ) )  )
}
#------------------------------------------------------------------------------

modelo_xgboost_actual <- function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, 
                                   pcolsample_bytree, peta, palpha, plambda, pgamma, pmin_child_weight, pmax_depth )
{

  #La gran llamada a  XGBoost
  modelo = xgb.train( 
                      data = ptrain,  
                      missing = NA,
                      objective="binary:logistic",
                      watchlist = list( train=ptrain, test=ptest ),
                      feval = fganancia_logistic_xgboost,  maximize= TRUE, 
                      nround= env$xgboost$nround_max,
                      early_stopping_rounds= env$xgboost$nround_early_stopping,
                      tree_method = "hist",
                      max_bin = env$xgboost$max_bin,
                      base_score = mean( getinfo(ptrain, "label") ) ,
                      subsample = env$xgboost$subsample, 
                      colsample_bytree = pcolsample_bytree, 
                      eta = peta,
                      min_child_weight = pmin_child_weight, 
                      max_depth = pmax_depth,
                      alpha = palpha, lambda = plambda, gamma = pgamma,
                     )
                     
                     
  iteracion_max      <- which.max( unlist(modelo$evaluation_log[ , test_ganancia]) )
  ganancia           <- unlist( modelo$evaluation_log[ , test_ganancia] )[ iteracion_max ] 
  
  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest)

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( testing_prediccion,  getinfo(ptest, "label") ) 

  tb_importancia <-  as.data.table( xgb.importance( dimnames(ptrain)[[2]], modelo, trees=0:(iteracion_max-1) ) )

  vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )

  return( list("ganancia_test"=ganancia, 
               "auc_test"=auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "nround"= iteracion_max,
               "importancia"=tb_importancia ) )
}
#------------------------------------------------------------------------------

modelo_xgboost_futuro <- function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, 
                                   pcolsample_bytree, peta, palpha, plambda, pgamma, pmin_child_weight, pmax_depth, pnround )
{

  #La gran llamada a  XGBoost
  modelo = xgb.train( 
                      data = ptrain,  
                      missing = NA,
                      objective="binary:logistic",
                      watchlist = list( train=ptrain, test=ptest ),
                      feval = fganancia_logistic_xgboost,  maximize= TRUE, 
                      nround= pnround,
                      tree_method = "hist",
                      max_bin = env$xgboost$max_bin,
                      base_score = mean( getinfo(ptrain, "label") ) ,
                      subsample = env$xgboost$subsample, 
                      colsample_bytree = pcolsample_bytree, 
                      eta = peta,
                      min_child_weight = pmin_child_weight, 
                      max_depth = pmax_depth,
                      alpha = palpha, lambda = plambda, gamma = pgamma,
                     )
                     
                     
  iteracion_max      <- pnround
  ganancia           <- unlist( modelo$evaluation_log[ , test_ganancia] )[ iteracion_max ] 
  
  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest)

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( testing_prediccion,  getinfo(ptest, "label") ) 

  tb_importancia <-  as.data.table( xgb.importance( dimnames(ptrain)[[2]], modelo, trees=0:(iteracion_max-1) ) )

  vcanaritos_muertos <-  sum( tb_importancia$Feature %like% "canarito" )

  return( list("ganancia_test"=ganancia, 
               "auc_test"=auc, 
               "canaritos_muertos"=  vcanaritos_muertos, 
               "nround"= iteracion_max,
               "importancia"=tb_importancia ) )
}
#------------------------------------------------------------------------------

glob_ganancia_mejor <- 0
glob_nround_mejor   <- 0

modelo_xgboost_ganancia_MBO_directo <- function( x )
{
  gc()
  t0   <-  Sys.time()

  
  dactual_train <-   xgb.DMatrix( data  = data.matrix( dataset_grande[ (sample < env$undersampling | clase_ternaria==1) & mes<=(env$data$mes_actual_train+x$pventana-1)  &  mes>=env$data$mes_actual_train & mes!=env$data$mes_actual_test, 
                                                                       !c("sample",env$data$campo_id, env$data$clase_nomcampo), 
                                                                       with=FALSE]),
                                  label = dataset_grande[ (sample < env$undersampling | clase_ternaria==1) & mes<=(env$data$mes_actual_train+x$pventana-1)  &  mes>=env$data$mes_actual_train & mes!=env$data$mes_actual_test,
                                                          get(  env$data$clase_nomcampo) ] ,
                                  missing=NA 
                                )

  mactual  <- modelo_xgboost_actual(
                             dactual_train,  
                             dactual_test, 
                             env$data$clase_nomcampo, 
                             env$data$clase_valor_positivo, 
                             env$problema, 
                             pcolsample_bytree= x$pcolsample_bytree,  
                             peta= x$peta, 
                             palpha= x$palpha,
                             plambda= x$plambda,
                             pgamma= x$pgamma,
                             pmin_child_weight= x$pmin_child_weight,
                             pmax_depth= x$pmax_depth
                            )


 #Aqui esta el error,  en el futuro NO se hace UNDERSAMPLING
  dfuturo_train <-   xgb.DMatrix( data  = data.matrix(   dataset_grande[ mes<=(env$data$mes_futuro_train+x$pventana-1)  &  mes>=env$data$mes_futuro_train & mes!=env$data$mes_futuro_test,
                                                                       !c("sample",env$data$campo_id, env$data$clase_nomcampo), 
                                                                       with=FALSE]),
                                  label = dataset_grande[ (sample < env$undersampling | clase_ternaria==1) & mes<=(env$data$mes_futuro_train+x$pventana-1)  &  mes>=env$data$mes_futuro_train & mes!=env$data$mes_futuro_test,
                                                          get(  env$data$clase_nomcampo) ] ,
                                  missing=NA 
                                )


  mfuturo  <- modelo_xgboost_futuro(
                             dfuturo_train,  
                             dfuturo_test, 
                             env$data$clase_nomcampo, 
                             env$data$clase_valor_positivo, 
                             env$problema, 
                             pcolsample_bytree= x$pcolsample_bytree,  
                             peta= x$peta, 
                             palpha= x$palpha,
                             plambda= x$plambda,
                             pgamma= x$pgamma,
                             pmin_child_weight= x$pmin_child_weight,
                             pmax_depth= x$pmax_depth,
                             pnround= mactual$nround                             
                            )



  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("ventana=", x$pventana,                   ", ",
                        "nround=",  mactual$nround,                ", ",
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

  #Grabo la importancia de las variables
  if( mactual$ganancia_test >  glob_ganancia_mejor )
  {
    glob_ganancia_mejor  <<- mactual$ganancia_test
    glob_nround_mejor    <<- mactual$nround
    
    setwd( env$directory$work )
    fwrite(  mactual$importancia,
             file = paste0( "importancia_", env$experimento,".txt"),
             sep="\t" )
  }

  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

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


#cargo los archivos de entrada
setwd( env$directory$datasets)
dataset_grande   <- fread(cmd=paste("cat", env$data$archivo_grande))

dataset_grande <- dataset_grande[ foto_mes>=env$data$mes_primero  & foto_mes<=env$data$mes_ultimo, ]
gc()

#Borro campos
if( length(env$data$campos_a_borrar)>0 )  dataset_grande[ ,  (env$data$campos_a_borrar) := NULL    ] 

set.seed(410551)
#agrego variable para el undersampling
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]


#agrego las variables canarito
agregar_canaritos( dataset_grande, 34 )


#agrego la columna  mes_actual
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))
tbl <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tbl ) <- c( "mes", "foto_mes" )

dataset_grande[  tbl,  on="foto_mes",  mes:= i.mes ]


#dejo la clase en {0,1}  clase  binaria1
dataset_grande[, (env$data$clase_nomcampo) := as.numeric( get(env$data$clase_nomcampo) == env$data$clase_valor_positivo  )] 


#genero el formato requerido por XGBoost los datasets de testing
#ya que son siempre los mismos independientemente del tamano de la ventana

dactual_test  <-   xgb.DMatrix( data  = data.matrix( dataset_grande[mes==env$data$mes_actual_test, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                label = dataset_grande[ mes==env$data$mes_actual_test, get(env$data$clase_nomcampo)], 
                                missing=NA 
                              )

dfuturo_test  <-   xgb.DMatrix( data  = data.matrix( dataset_grande[mes==env$data$mes_futuro_test, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                label = dataset_grande[ mes==env$data$mes_futuro_test, get(env$data$clase_nomcampo)], 
                                missing=NA 
                              )


hiperparametros_crear(env$hiper)


configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  modelo_xgboost_ganancia_MBO_directo

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
        name = "prueba",
        fn   = funcion_optimizar,
        par.set = makeParamSet(
            makeNumericParam("pcolsample_bytree" ,  lower=0.05    , upper=   1.0),
            makeNumericParam("peta"              ,  lower=0.0     , upper=   0.3),
            makeNumericParam("palpha"            ,  lower=0.0     , upper=  50.0),
            makeNumericParam("plambda"           ,  lower=0.0     , upper=  50.0),
            makeNumericParam("pgamma"            ,  lower=0.0     , upper=  20.0),
            makeNumericParam("pmin_child_weight" ,  lower=0.0     , upper= 100.0),
            makeIntegerParam("pmax_depth"        ,  lower=2L      , upper=  20L),
            makeIntegerParam("pventana"          ,  lower=1L      , upper=  12L)
        ),
        has.simple.signature = FALSE,
        global.opt.value = -1
        )



ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = env$mbo$saveondisk_time,  save.file.path = env$mbo$archivo_trabajo)
ctrl <-  setMBOControlTermination(ctrl, iters = env$mbo$iteraciones )
ctrl <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = env$mbo$iteraciones)

surr.km <-  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))


setwd(env$directory$work)
if(!file.exists(env$mbo$archivo_trabajo))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( env$mbo$archivo_trabajo )
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aplico el mejor modelo que encontro la  Optimizacion Bayesiana
#A los datos de 201906, los que tengo que predecir !!!

#Aqui no hago undersampling, voy a entrenar con TODO el dataset
#run$x$pventana  tiene la cantidad de meses optimos en los que hay que entrenar
train_final  <-  dataset_grande[ foto_mes <= env$data$mes_ultimo_conclase  &  mes >= run$x$pventana+3-1, ]


dtrain_final  <-   xgb.DMatrix( data  = data.matrix( train_final[, !c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE]),
                                label = train_final[ , get(env$data$clase_nomcampo)], 
                                missing=NA 
                              )


mfinal = xgb.train( 
                    data = dtrain_final,  
                    missing = NA,
                    objective="binary:logistic",
                    feval = fganancia_logistic_xgboost,  maximize= TRUE, 
                    nround= glob_nround_mejor,
                    tree_method = "hist",
                    max_bin = env$xgboost$max_bin,
                    base_score = mean( getinfo(dtrain_final, "label") ) ,
                    subsample = env$xgboost$subsample, 
                    colsample_bytree = run$x$pcolsample_bytree, 
                    eta = run$x$peta,
                    min_child_weight = run$x$pmin_child_weight, 
                    max_depth = run$x$pmax_depth,
                    alpha = run$x$palpha, 
                    lambda = run$x$plambda, 
                    gamma = run$x$pgamma,
                  )


#Los datos de 201906,  que no tienen clase
test_final <- dataset_grande[  foto_mes== env$data$mes_ultimo, ]

#aplico el modelo a datos nuevos
testing_prediccion  <- predict( mfinal,  
                                data.matrix(test_final[,!c("sample",env$data$campo_id, env$data$clase_nomcampo), with=FALSE ] ))

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




#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

