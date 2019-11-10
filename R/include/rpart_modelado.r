#------------------------------------------------------------------------------

modelo_rpart_pcorte = function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, pmaxdepth, pminbucket, pminsplit, pcp, ppcorte )
{

  formula  <-  formula(paste(pclase_nomcampo, "~ ."))

  modelo   <-  rpart(formula,   data = ptrain,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp)


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest, type = "prob")


  # calculo la ganancia normalizada  en testing
  gan  <-  fmetrica_ganancia_rpart_prob(testing_prediccion[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], ppcorte, pclase_valor_positivo, pproblema)
  # calculo el AUC en testing
  auc  <-  fmetrica_auc_rpart(testing_prediccion[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo )


  return( list("ganancia_test"=gan, "auc_test"=auc) )
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
  #Divido el dataset en training 70% y testing 30%  , usando la libreria dplyr
  dataset  <- as.data.table(mutate(pdataset, idtempo =  row_number()))

  set.seed(psemilla )
  dataset_training <- as.data.table(dataset %>%
  group_by(!!as.name(pclase_nomcampo)) %>%
  sample_frac(env$montecarlo$training_prob) %>%
  ungroup)
  dataset_testing  <- as.data.table(anti_join(dataset, dataset_training, by = "idtempo"))

  dataset_training[ ,  idtempo := NULL    ] 
  dataset_testing[ ,  idtempo := NULL    ] 


  res <-  modelo_rpart( dataset_training, dataset_testing, pclase_nomcampo, pclase_valor_positivo, pproblema, pmaxdepth, pminbucket, pminsplit, pcp  )

  rm( dataset_training )
  rm( dataset_testing )
  gc()

  return( res )
}
#------------------------------------------------------------------------------

#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia_MBO_montecarlo = function(x = list(pmaxdepth, pminbucket, pminsplit, pcp, pmeses_cantidad ) )
{

  #calculo el minbucket que corresponde
  vminbucket  <-   round(x$pminbucket * x$pminsplit)

  t0   <-  Sys.time()

  mactual  <-  lapply( env$montecarlo$semilla_azar,  modelo_rpart_ganancia_MBO_semilla, 
                       dataset$actual, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, 
                       pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, 
                       pcp=x$pcp )


  mactual  <-  rbindlist( mactual )

  mfuturo  <-  modelo_rpart( dataset$futuro_train,  dataset$futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )

  
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
                          pfuturo_ganancia= mfuturo$ganancia_test, 
                          pfuturo_auc= mfuturo$auc_test,
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


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

modelo_rpart_ganancia_MBO_directo = function(x = list(pmaxdepth, pminbucket, pminsplit, pcp, pmeses_cantidad ) )
{

  #calculo el minbucket que corresponde
  vminbucket  <-   round(x$pminbucket * x$pminsplit)

  t0   <-  Sys.time()

  mactual  <-  modelo_rpart( dataset$actual_train,  dataset$actual_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )

  mfuturo  <-  modelo_rpart( dataset$futuro_train,  dataset$futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )

  
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
                          pactual_ganancia= mactual$ganancia_test, 
                          pactual_auc= mactual$auc_test,
                          pfuturo_ganancia= mfuturo$ganancia_test, 
                          pfuturo_auc= mfuturo$auc_test,
                          ptiempo= tiempo_corrida,
                          pprob_corte= env$problema$prob_corte, 
                          pst_parametros= st_parametros, 
                          parchivo_actual_train= env$data$archivo_actual_train,
                          parchivo_actual_test=  env$data$archivo_actual_test,
                          parchivo_futuro_train= env$data$archivo_futuro_train,
                          parchivo_futuro_test=  env$data$archivo_futuro_test,
                          pclase="ternaria",
                          pcanaritos_muertos= mfuturo$canaritos_muertos
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

modelo_rpart_ganancia_MBO_meses1 = function(x = list(pmaxdepth, pminbucket, pminsplit, pcp, pmeses_cantidad ) )
{

  #calculo el minbucket que corresponde
  vminbucket  <-   round(x$pminbucket * x$pminsplit)

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )

  mactual  <-  modelo_rpart( vds$actual_train,  dataset$actual_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )
  mfuturo  <-  modelo_rpart( vds$futuro_train,  dataset$futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp )

  
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
                          pactual_ganancia= mactual$ganancia_test, pactual_auc= mactual$auc_test,
                          pfuturo_ganancia= mfuturo$ganancia_test, pfuturo_auc= mfuturo$auc_test,
                          ptiempo= tiempo_corrida,
                          pprob_corte= env$problema$prob_corte, 
                          pst_parametros= st_parametros, 
                          parchivo_actual_train= vds$actual_train_st,
                          parchivo_actual_test= paste( env$data$ldata$mes_actual_test,  env$data$ldata$mes_sufijo, sep=""), 
                          parchivo_futuro_train= vds$futuro_train_st, 
                          parchivo_futuro_test= paste( env$data$ldata$mes_futuro_test,  env$data$ldata$mes_sufijo, sep=""),
                          pclase="ternaria"
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  #borro los datasets intermedios creados
  rm(ds)
  gc()

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

x <- list( "pmaxdepth"=2, "pminbucket"=0.3, "pminsplit"=50, "pcp"=0, "pmeses_cantidad"=3, "ppcorte"=0.025 )

modelo_rpart_ganancia_MBO_baja12 = function(x = list(pmaxdepth, pminbucket, pminsplit, pcp, pmeses_cantidad, ppcorte ) )
{

  #calculo el minbucket que corresponde
  vminbucket  <-   round(x$pminbucket * x$pminsplit)

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )

  vds$actual_train[ , (env$data$clase_nomcampo) := ifelse( get(env$data$clase_nomcampo) %in% env$data$clases_juntar, 'BAJA+2', 'CONTINUA' ) ]
  vds$futuro_train[ , (env$data$clase_nomcampo) := ifelse( get(env$data$clase_nomcampo) %in% env$data$clases_juntar, 'BAJA+2', 'CONTINUA' ) ]
 

  mactual  <-  modelo_rpart_pcorte( vds$actual_train,  dataset$actual_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp, ppcorte=x$ppcorte )
  mfuturo  <-  modelo_rpart_pcorte( vds$futuro_train,  dataset$futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp, ppcorte=x$ppcorte )

  
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
                          pactual_ganancia= mactual$ganancia_test, pactual_auc= mactual$auc_test,
                          pfuturo_ganancia= mfuturo$ganancia_test, pfuturo_auc= mfuturo$auc_test,
                          ptiempo= tiempo_corrida,
                          pprob_corte= x$ppcorte, 
                          pst_parametros= st_parametros, 
                          parchivo_actual_train= vds$actual_train_st,
                          parchivo_actual_test= paste( env$data$ldata$mes_actual_test,  env$data$ldata$mes_sufijo, sep=""), 
                          parchivo_futuro_train= vds$futuro_train_st, 
                          parchivo_futuro_test= paste( env$data$ldata$mes_futuro_test,  env$data$ldata$mes_sufijo, sep=""),
                          pclase="binaria2"
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  #borro los datasets intermedios creados
  gc()

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------
