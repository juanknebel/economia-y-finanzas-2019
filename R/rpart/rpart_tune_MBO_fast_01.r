#MBO  Hyperparameter Tuning

#limpio la memoria
rm(list=ls())
gc()



library("rpart")
library("data.table")

library("DiceKriging")
library("mlrMBO")

library("parallel")

#raiz del environment
env <- list()

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
env$directory <- directory


env$experimento          <-  1290


#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c(data$campo_id)
data$archivo_actual_train <-  "201902_dias.txt"
data$archivo_actual_test  <-  "201902_dias.txt"
data$archivo_futuro_train <-  "201902_dias.txt"
data$archivo_futuro_test  <-  "201904_dias.txt"
env$data <- data


#Parametros  Montecarlo 
montecarlo <- list()
montecarlo$training_prob        <-  0.70
montecarlo$semilla_azar         <-  c(102191, 200177, 410551, 552581, 892237)
env$montecarlo  <-  montecarlo


#Parametros  mlrMBO
mbo <- list()
mbo$iteraciones       <-  100
mbo$saveondisk_time   <-  600   # cada 600 segundos guarda a disco cuanto avanzo
mbo$archivo_trabajo   <-  paste("exp_", env$experimento, ".RDATA", sep="") 
env$mbo <- mbo


#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_imagen    <-  paste( "exp_", env$experimento, ".jpg", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory_work
hiper$clase_tipo     <-  "ternaria"
hiper$programa       <-  "rpart_tune_MBO_01.r"
hiper$algoritmo      <-  "rpart"
hiper$busqueda       <-  "MBO"
hiper$estimacion     <-  "Montecarlo"
hiper$observaciones  <-  "5 semillas"
hiper$separador      <-  "\t"
hiper$directory      <-  env$directory$work
env$hiper <-  hiper


problema <- list()
problema$prob_corte           <-      0.025
problema$ganancia_acierto     <-  19500 
problema$ganancia_noacierto   <-   -500
env$problema <- problema


#Hacer que la variable   env   NO se pueda modificar
lockBinding( "env", globalenv() )



#------------------------------------------------------

hiperparametros_titulos = function( phiper, parchivo )
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

hiperparametros_crear = function( phiper)
{
  hiperparametros_titulos( phiper, phiper$arch_global ) 
  hiperparametros_titulos( phiper, phiper$arch_local ) 
}
#------------------------------------------------------


hiperparametros_grabar1 = function( phiper, parchivo,
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

hiperparametros_grabar = function( phiper,
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

fmetrica_ganancia_rpart  = function( probs, clases, pclase_valor_positivo, problema )
{
 
  return(  sum(    (probs > problema$prob_corte  ) * 
                   ifelse( clases== pclase_valor_positivo, problema$ganancia_acierto, problema$ganancia_noacierto )   
              )
         )
}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == pclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

modelo_rpart = function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, pmaxdepth, pminbucket, pminsplit, pcp )
{

  formula  <-  formula(paste(pclase_nomcampo, "~ ."))

  modelo   <-  rpart(formula,   data = ptrain,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp)


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest, type = "prob")


  # calculo la ganancia normalizada  en testing
  gan  <-  fmetrica_ganancia_rpart(testing_prediccion[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo, pproblema)
  # calculo el AUC en testing
  auc  <-  fmetrica_auc_rpart(testing_prediccion[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo )

  #cuento cuantas variables canarito distintas aparecen
  frame  <- modelo$frame
  leaves <- frame$var == "<leaf>"
  used   <- unique(frame$var[!leaves])
  canaritos_muertos <- sum( unlist( used ) %like% "canarito" )


  return( list("ganancia_test"=gan, "auc_test"=auc, "canaritos_muertos"=  canaritos_muertos) )
}
#------------------------------------------------------------------------------

modelo_rpart_ganancia_MBO_semilla = function( psemilla, pdataset, pclase_nomcampo, pclase_valor_positivo, pproblema, pmaxdepth, pminbucket, pminsplit, pcp  )
{
  #Divido el dataset en training 70% y testing 30%
  set.seed( psemilla )
  tb_clase <-  as.data.table(  pdataset[ , get(env$data$clase_nomcampo) ] )
  colnames( tb_clase ) <- c("clase" )
  tb_clase[  , c("azar","idtempo" ) := list( runif( nrow(tb_clase) ), .I ) ]
  
  setorder( tb_clase, clase, azar )
  tb_clase[  , training := (.I %% 10)<7 ]
  
  setorder( tb_clase, idtempo )
  pdataset[  , training:=  tb_clase$training ]
  
  dataset_training <- pdataset[ (training) ]
  dataset_testing  <- pdataset[ !(training) ]

  res <-  modelo_rpart( dataset_training, dataset_testing, pclase_nomcampo, pclase_valor_positivo, pproblema, pmaxdepth, pminbucket, pminsplit, pcp  )

  rm( tb_clase, dataset_training, dataset_testing )
  gc()

  return( res )
}
#------------------------------------------------------------------------------
#corre  rpart  usando  las semillas, y promedia el resultado
x <- list(  pmaxdepth= 23,
            pminbucket= 11,
            pminsplit= 24,
            pcp= -0.000891174876020549
         )

modelo_rpart_ganancia_MBO_montecarlo = function(x = list(pmaxdepth, pminbucket, pminsplit, pcp ) )
{

  #calculo el minbucket que corresponde
  vminbucket  <-   round(x$pminbucket * x$pminsplit)

  t0   <-  Sys.time()

  mactual  <- mclapply( env$montecarlo$semilla_azar,  modelo_rpart_ganancia_MBO_semilla, 
                        dataset_actual, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, 
                        pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, 
                        pcp=x$pcp,
                        mc.cores=5 )


  mactual  <-  rbindlist( mactual )

  mfuturo  <-  modelo_rpart( dataset_futuro_train,  dataset_futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("xval=",      0,           ", " ,
                        "maxdepth=",  x$pmaxdepth, ", " ,
                        "minbucket=", vminbucket,  ", " ,
                        "minsplit=",  x$pminsplit, ", " ,
                        "cp=",        x$pcp,
                        sep = ""
                       )

  #escribo al archivo de salida los resultados de esta corrida
  hiperparametros_grabar( phiper= env$hiper,
                          pactual_ganancia= mean(mactual$ganancia_test)/(1 - env$montecarlo$training_prob), 
                          pactual_auc= mean(mactual$auc_test),
                          pactual_canaritos= mean(mactual$canaritos_muertos),
                          pfuturo_ganancia= mfuturo$ganancia_test, 
                          pfuturo_auc= mfuturo$auc_test,
                          pfuturo_canaritos= mean(mfuturo$canaritos_muertos),
                          ptiempo= tiempo_corrida,
                          pprob_corte= env$problema$prob_corte, 
                          pst_parametros= st_parametros, 
                          parchivo_actual_train= env$data$archivo_actual_train,
                          parchivo_actual_test=  env$data$archivo_actual_test,
                          parchivo_futuro_train= env$data$archivo_futuro_train,
                          parchivo_futuro_test=  env$data$archivo_futuro_test,
                          pclase="ternaria",
                          pcanaritos_muertos= mean( mfuturo$canaritos_muertos )
                        )


  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------


tipo_os <- Sys.info()[['sysname']]

#cargo los archivos de entrada
setwd( env$directory$datasets)
if( tipo_os=="Linux" ) { dataset_actual  <- fread(cmd=paste( "cat ", env$data$archivo_actual_train)) } else 
{ dataset_actual  <- fread(env$data$archivo_actual_train) }

#borro las variables que no me interesan
dataset_actual[,  (env$data$campos_a_borrar) := NULL    ] 



if( tipo_os=="Linux" ) { dataset_futuro_test  <- fread(cmd=paste( "cat ", env$data$archivo_futuro_test)) } else 
{ dataset_futuro_test  <- fread(env$data$archivo_futuro_test) }

dataset_futuro_test[,  (env$data$campos_a_borrar) := NULL    ] 


if( tipo_os=="Linux" ) { dataset_futuro_train  <- fread(cmd=paste( "cat ", env$data$archivo_futuro_train)) } else 
{ dataset_futuro_train  <- fread(env$data$archivo_futuro_train) }

dataset_futuro_train[,  (env$data$campos_a_borrar) := NULL    ] 


magic_canaritos <- 0.1 
canaritos_cantidad <- as.integer( round(ncol(dataset_actual) * magic_canaritos) )
for( i in 1:canaritos_cantidad )
{
  dataset_actual[        , paste0( "canarito", i ) :=  runif( nrow(dataset_actual) ) ]
  dataset_futuro_train[  , paste0( "canarito", i ) :=  runif( nrow(dataset_futuro_train) ) ]
  dataset_futuro_test[   , paste0( "canarito", i ) :=  runif( nrow(dataset_futuro_test) ) ] 
}




hiperparametros_crear(env$hiper)


configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  modelo_rpart_ganancia_MBO_montecarlo

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
        name = "prueba",
        fn   = funcion_optimizar,
        par.set = makeParamSet(
            makeIntegerParam("pmaxdepth"         ,  lower=3L    , upper=30L),
            makeNumericParam("pminbucket"        ,  lower=0.1   , upper=0.5),
            makeIntegerParam("pminsplit"         ,  lower=1L    , upper=100L),
            makeNumericParam("pcp"               ,  lower=0.0   , upper= 0.01)
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




#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

