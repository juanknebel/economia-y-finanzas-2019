#MBO  Hyperparameter Tuning

#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("ROCR")
library("ranger")
library("randomForest")  #solo se usa para imputar nulos

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


env$experimento          <-  2501

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


#estos valores se graban en el archivo de salida
hiper <- list()
hiper$experimento    <-  env$experimento
hiper$arch_global    <-  "hiperparametro_GLOBAL.txt"
hiper$arch_local     <-  paste( "hiperparametro_", env$experimento, ".txt", sep="" )
hiper$arch_imagen    <-  paste( "exp_", env$experimento, ".jpg", sep="" )
hiper$arch_over      <-  paste( "exp_", env$experimento, "_over.jpg", sep="" )
hiper$directory      <-  env$directory_work
hiper$clase_tipo     <-  "ternaria"
hiper$programa       <-  "ranger_tune_MBO_meses_undersampling.r"
hiper$algoritmo      <-  "ranger"
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
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc  <- function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == pclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

modelo_ranger <- function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, pnum.trees, pmin.node.size, pmtry )
{
  formula  <-  formula(paste0(pclase_nomcampo, "  ~ . -sample", " -", env$data$campo_id))
  ptrain <- droplevels(ptrain)
  
  modelo   <-  ranger(formula,   
                      data = ptrain,
                      probability=TRUE,
                      splitrule= "gini", 
                      importance= "impurity",
                      num.trees= pnum.trees,
                      min.node.size= pmin.node.size,
                      mtry= pmtry )


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest)

  
  # calculo la ganancia normalizada  en testing
  gan  <-  fmetrica_ganancia(testing_prediccion$predictions[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo, problema)
  # calculo el AUC en testing
  auc  <-  fmetrica_auc(testing_prediccion$predictions[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo )

  vcanaritos_muertos <-  sum( names(modelo$variable.importance) %like% "canarito" )

  return( list("ganancia_test"=gan, "auc_test"=auc, "canaritos_muertos"=  vcanaritos_muertos, "importancia"=modelo$variable.importance ) )
}
#------------------------------------------------------------------------------

glob_ganancia_mejor <- 0


modelo_ranger_ganancia_MBO_directo <- function( x )
{

  gc()
  
  t0   <-  Sys.time()

  mactual  <-  modelo_ranger(dataset_grande[ (sample < env$undersampling | clase_ternaria=="BAJA+2") & mes<=(env$data$mes_actual_train+x$pventana-1)  &  mes>=env$data$mes_actual_train & mes!=env$data$mes_actual_test,],  
                             dataset_grande[ mes==env$data$mes_actual_test, ], 
                             env$data$clase_nomcampo, 
                             env$data$clase_valor_positivo, 
                             env$problema, 
                             pnum.trees= x$pnum.trees,  
                             pmin.node.size= x$pmin.node.size, 
                             pmtry= x$pmtry)
                             
                             
  mfuturo  <-  modelo_ranger( dataset_grande[ (sample < env$undersampling | clase_ternaria=="BAJA+2") & mes<=(env$data$mes_futuro_train+x$pventana-1)  &  mes>=env$data$mes_futuro_train & mes!=env$data$mes_futuro_test,],  
                             dataset_grande[ mes==env$data$mes_futuro_test, ], 
                             env$data$clase_nomcampo, 
                             env$data$clase_valor_positivo, 
                             env$problema, 
                             pnum.trees= x$pnum.trees,  
                             pmin.node.size= x$pmin.node.size, 
                             pmtry= x$pmtry)

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("ventana=", x$pventana, ", ",
                        "probability=TRUE", ", " ,
                        "splitrule=gini",   ", " ,
                        "num.trees=",      x$pnum.trees,     ", " ,
                        "min.node.size=",  x$pmin.node.size, ", " ,
                        "mtry=",           x$pmtry,          ", " ,
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
    
    tb_importancia <- as.data.table(cbind( names(mactual$importancia), mactual$importancia))
    colnames( tb_importancia )  <-  c("Feature", "Importance" )
    tb_importancia[ , Importance:= as.numeric( Importance ) ]
    setorder( tb_importancia, -Importance )
    fwrite( tb_importancia,
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
dataset_grande   <- fread(cmd=paste("cat", env$data$archivo_grande), stringsAsFactors=TRUE)

dataset_grande <- dataset_grande[ foto_mes>=env$data$mes_primero  & foto_mes<=env$data$mes_ultimo, ]
gc()

#Borro campos
if( length(env$data$campos_a_borrar)>0 )  dataset_grande[ ,  (env$data$campos_a_borrar) := NULL    ] 

set.seed(410551)
#agrego variable para el undersampling
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]

#imputo los nulos, ya que ranger no acepta nulos
dataset_grande <-  na.roughfix( dataset_grande )
gc()

#agrego las variables canarito
agregar_canaritos( dataset_grande, 34 )


#agrego la columna  mes_actual
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))
tbl <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tbl ) <- c( "mes", "foto_mes" )

dataset_grande[  tbl,  on="foto_mes",  mes:= i.mes ]


hiperparametros_crear(env$hiper)


configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  modelo_ranger_ganancia_MBO_directo

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
        name = "prueba",
        fn   = funcion_optimizar,
        par.set = makeParamSet(
            makeIntegerParam("pnum.trees"        ,  lower=1L    , upper= 500L),
            makeIntegerParam("pmin.node.size"    ,  lower=1L    , upper= 100L),
            makeIntegerParam("pmtry"             ,  lower=1L    , upper=  20L),
            makeIntegerParam("pventana"          ,  lower=1L    , upper=  12L)
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
train_final  <- droplevels(train_final)

formula  <-  formula(paste0(env$data$clase_nomcampo, "  ~ . -sample ", " -", env$data$campo_id))

#Genero el modelo  
mfinal   <-  ranger(formula,   
                    data = train_final,
                    probability=TRUE,
                    splitrule= "gini", 
                    importance= "impurity",
                    num.trees= run$x$pnum.trees,
                    min.node.size= run$x$pmin.node.size,
                    mtry= run$x$pmtry )


#Los datos de 201906,  que no tienen clase
test_final <- dataset_grande[  foto_mes== env$data$mes_ultimo, ]

#aplico el modelo a datos nuevos
testing_prediccion  <- predict( mfinal,  test_final)

#pego los numero_de_cliente  con la probabilidad
tb_prediccion <-  as.data.table( cbind(   test_final[ , get(env$data$campo_id) ] , testing_prediccion$predictions[, env$data$clase_valor_positivo ] ) )
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

