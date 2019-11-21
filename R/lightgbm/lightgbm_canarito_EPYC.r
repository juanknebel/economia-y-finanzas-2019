#MBO  Hyperparameter Tuning

#limpio la memoria
rm(list=ls())
gc()



library("rpart")
library("data.table")
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


env$experimento          <-  3602
env$canaritos_idx        <-     0.2

#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c(data$campo_id)
data$archivo_grande       <-  "paquete_premium_dias.txt"
data$mes_futuro_test      <-  1
data$mes_futuro_train     <-  3
data$mes_actual_test      <-  3
data$mes_actual_train     <-  5
data$mes_primero          <-  201712
data$mes_ultimo           <-  201904
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
hiper$clase_tipo     <-  "binaria1"
hiper$programa       <-  "lightgbm_tune_MBO_meses.r"
hiper$algoritmo      <-  "lightgbm"
hiper$busqueda       <-  "MBO"
hiper$estimacion     <-  "mes_futuro"
hiper$observaciones  <-  "ventana"
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
#dibujo la evolucion de la metrica

hiperparametros_refrescar  = function( phiper )
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

fmetrica_ganancia_lightgbm  = function( pprob_corte, probs, clases, problema )
{
 
  res <-  sum(    (probs > pprob_corte  ) * 
                   ifelse( clases== 1, problema$ganancia_acierto, problema$ganancia_noacierto ) 
                   , na.rm = TRUE  
              )

  return(  ifelse(  is.na(res) , 0, res )  
         )

}
#------------------------------------------------------------------------------

#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_lightgbm  = function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------------------------------

fganancia_logistic_actual   <- function(probs, clases) 
{
   vlabels <- getinfo(clases, "label")

   unidos <-  cbind( probs, runif(length(vlabels)), vlabels )
   unidos <- as.data.table( unidos )
   names( unidos ) <- c( "prob", "azar", "clase" )
   gan <-  sum(  unidos[  , (prob>0.025)*ifelse( clase==1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto ) ])
   

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan) , 0, gan) ,
                  higher_better= TRUE 
                )
         )
}
#------------------------------------------------------------------------------

modelo_lightgbm_valid = function( ptrain, x  )
{
  #genero el formato requerido por LightGBM
  dactual_train  <-   lgb.Dataset( data  = data.matrix(ptrain[ ,setdiff(names(ptrain),env$data$clase_nomcampo), with=FALSE ]),
                                   label = ptrain[, get(env$data$clase_nomcampo)], 
                                   free_raw_data=FALSE )


  mactual = lgb.train( 
                     data= dactual_train,
                     objective= "binary",
                     valids= list( valid= dactual_validacion),
                     metric= "auc",
                     eval = fganancia_logistic_actual,
                     seed= 102191,
                     early_stopping_round = 50,
                     num_iterations= 2000,
                     boost_from_average= TRUE,
                     bagging_fraction= 1, 
                     max_bin= 255, 
                     num_leaves= 1024,
                     num_canaritos= 34,
                     verbose= -1,
                     min_data_in_leaf= x$pmin_data_in_leaf,
                     learning_rate= x$plearning_rate, 
                     feature_fraction= x$pfeature_fraction, 
                     lambda_l1 = x$plambda_l1,
                     lambda_l2 = x$plambda_l2
                    )

  
  iteracion_max  <- which.max(  unlist( mactual$record_evals$valid$ganancia$eval ) ) 


  gan_validate <-  unlist( mactual$record_evals$valid$ganancia$eval)[ iteracion_max ]
  auc_validate <-  unlist( mactual$record_evals$valid$auc$eval)[ iteracion_max ]

  tb_importancia <-  as.data.table( lgb.importance( model = mactual ) )
  canaritos_muertos <-  length(unique(tb_importancia[ Feature %like% "canarito", Feature ] ))

  return( list("ganancia_test"=gan_validate, "auc_test"=auc_validate, "iteracion"=  iteracion_max, "canaritos_muertos"= canaritos_muertos) )
}
#------------------------------------------------------------------------------

modelo_lightgbm_puro = function( ptrain, x, piteracion  )
{
  dfuturo_train  <-   lgb.Dataset( data  = data.matrix(ptrain[ ,setdiff(names(ptrain),env$data$clase_nomcampo), with=FALSE ]),
                                   label = ptrain[, get(env$data$clase_nomcampo)], 
                                   free_raw_data=FALSE )


  mfuturo = lgb.train( 
                     data= dfuturo_train,
                     objective= "binary",
                     eval = fganancia_logistic_actual,
                     seed= 102191,
                     num_iterations= piteracion,
                     boost_from_average= TRUE,
                     bagging_fraction= 1, 
                     max_bin= 255, 
                     num_leaves= 1024,
                     num_canaritos= 34,
                     verbose= -1,
                     min_data_in_leaf= x$pmin_data_in_leaf,
                     learning_rate= x$plearning_rate, 
                     feature_fraction= x$pfeature_fraction, 
                     lambda_l1 = x$plambda_l1,
                     lambda_l2 = x$plambda_l2
                    )

  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  mfuturo, futuro_validacion_matrix )

  # calculo la ganancia
  gan_futuro <-  fmetrica_ganancia_lightgbm( 0.025, aplicacion_prediccion,  futuro_validacion_clase, env$problema ) 

 # calculo el AUC
  auc_futuro <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  futuro_validacion_clase ) 

  tb_importancia <-  as.data.table( lgb.importance( model = mfuturo ) )
  canaritos_muertos <-  length(unique(tb_importancia[ Feature %like% "canarito", Feature ] ))


  return( list("ganancia_test"=gan_futuro, "auc_test"=auc_futuro, "canaritos_muertos"= canaritos_muertos) )
}
#------------------------------------------------------------------------------


x <- list( pventana=2, pfeature_fraction=1.0, plearning_rate=0.02, pmin_data_in_leaf=20, plambda_l1=1, plambda_l2=2 )

modelo_lightgbm_ganancia_MBO_directo = function(x )
{

 
  t0   <-  Sys.time()

  mactual  <-  modelo_lightgbm_valid( dataset_grande[ mes<=(env$data$mes_actual_train+x$pventana-1)  &  mes>=env$data$mes_actual_train,],  
                                      x)

  mfuturo  <-  modelo_lightgbm_puro( dataset_grande[ mes<=(env$data$mes_futuro_train+x$pventana-1)  &  mes>=env$data$mes_futuro_train,],  
                                     x,
                                     mactual$iteracion)

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("feature_fraction=", x$pfeature_fraction,  ", " ,
                        "learning_rate=",    x$plearning_rate,     ", " ,
                        "min_data_in_leaf=", x$pmin_data_in_leaf,  ", " ,
                        "lambda_l1=",        x$plambda_l1,         ", " ,
                        "lambda_l2=",        x$plambda_l2,         ", " ,
                        "ventana=",          x$pventana,           ", " ,
                        "num_iterations=",   mactual$iteracion, 
                        sep = ""
                       )

  #escribo al archivo de salida los resultados de esta corrida
  hiperparametros_grabar( phiper= env$hiper,
                          pactual_ganancia= mean(mactual$ganancia_test), 
                          pactual_auc= mean(mactual$auc_test),
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
                          pclase="binaria1",
                          pcanaritos_muertos= -1
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

one_attribute_shake <- function( pdataset,  pgrupo, pcolumna, pid, pshake )
{
  tbl <-  pdataset[ , c( "id_shake", pgrupo, pcolumna), with=FALSE ]
  colnames( tbl ) <-  c( "id_shake", "grupo", "columna" )
  tbl[ , azar := runif( nrow( tbl ) ) ]
  setorderv(  tbl,  c("grupo", "columna", "azar") )
  tbl[ , azar := runif( nrow( tbl ) ) ]
  tbl[ , nshake :=  as.integer( round( rnorm( nrow(tbl), 0,  pshake))) ]
  
  #border absoluto
  tbl[ , nshake :=  ifelse( nshake+.I>0, nshake, -nshake ) ]
  tbl[ , nshake :=  ifelse( nshake+.I<=nrow(tbl), nshake, -nshake ) ]
  
  #border grupo
  tbl[ , nshake :=  ifelse( tbl[ .I + nshake, grupo ] == grupo , nshake, -nshake ) ]
  
  
  tbl[  , nuevo :=  tbl$columna[ .I + nshake ] ]
  
  
  pdataset[ tbl, on="id_shake",   (pcolumna)  := i.nuevo ]
  
}
#------------------------------------------------------------------------------

attributes_shake <- function( pdataset, pshake )
{
  vcolumnas <-  setdiff( colnames( pdataset ) , c(env$data$clase_nomcampo, "foto_mes", "mes") )
  vcolumnas <-  vcolumnas[ !(vcolumnas %like% "canarito") ]
  
  pdataset[ , id_shake := .I ]
  
  for( i in  1:length(vcolumnas) )
  {
    one_attribute_shake( pdataset, "foto_mes", vcolumnas[i], i, pshake )
  }
  
  pdataset[ , id_shake := NULL ]
}
#------------------------------------------------------------------------------

agregar_canaritos <- function( pdataset, pcanaritos_idx )
{

  canaritos_cantidad <- as.integer( round(ncol(pdataset) * pcanaritos_idx) )
  vcanaritos <-  paste0( "canarito", 1:canaritos_cantidad )

  #uso esta semilla para los canaritos
  set.seed(10217)

  #podria haber hecho un loop for
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

dataset_grande <- dataset_grande[ foto_mes>=env$data$mes_primero  & foto_mes<=env$data$mes_ultimo , ]
gc()

dataset_grande[ ,  (env$data$campos_a_borrar) := NULL    ] 


#agrego la columna  mes_actual
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))
tbl <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tbl ) <- c( "mes", "foto_mes" )
dataset_grande[  tbl,  on="foto_mes",  mes:= i.mes ]


#dejo la clase en {0,1}  clase  binaria1
dataset_grande[  , (env$data$clase_nomcampo) := as.integer(get(env$data$clase_nomcampo) == env$data$clase_valor_positivo ) ]


#Agrego los canaritos
agregar_canaritos( dataset_grande, env$canaritos_idx )


#------------------------------

#genero el formato requerido por LightGBM
dactual_validacion  <-   lgb.Dataset( data  = data.matrix(dataset_grande[ mes==env$data$mes_actual_test, setdiff(names(dataset_grande),env$data$clase_nomcampo), with=FALSE ]),
                                      label = dataset_grande[mes==env$data$mes_actual_test, get(env$data$clase_nomcampo)], 
                                      free_raw_data=FALSE )

futuro_validacion_matrix    <- as.matrix(dataset_grande[ mes==env$data$mes_futuro_test, setdiff(names(dataset_grande),env$data$clase_nomcampo), with=FALSE  ])
futuro_validacion_clase     <- dataset_grande[ mes==env$data$mes_futuro_test, get(env$data$clase_nomcampo) ]

#Recien aqui hago el shake de las variables
a1 <-  dataset_grande[ 10000, mcuentas_saldo ]

attributes_shake( dataset_grande,  10 )

a2 <-  dataset_grande[ 10000, mcuentas_saldo ]

a1
a2

#------------------------------
hiperparametros_crear(env$hiper)


for(  v in c(0.006 ) )
{
  cat( v, " "  )
  x <- list( pventana=12, pfeature_fraction=0.5, plearning_rate=v, pmin_data_in_leaf=1, plambda_l1=0.0, plambda_l2=0.0 )
  modelo_lightgbm_ganancia_MBO_directo(x)
}

#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

