#------------------------------------------------------------------------------

modelo_ranger_pcorte = function( ptrain, ptest, pclase_nomcampo, pclase_valor_positivo, pproblema, psemilla,
                                 pnum.trees, pmin.node.size, pmtry, psplitrule, ppcorte )
{

  formula  <-  formula(paste(pclase_nomcampo, "~ ."))


  modelo   <-  ranger( formula, data=ptrain, probability=TRUE, 
                       seed=psemilla,
                       num.trees=pnum.trees,  
                       min.node.size=pmin.node.size, 
                       mtry=pmtry, 
                       splitrule= psplitrule
                     )	


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict( modelo, ptest )


  # calculo la ganancia normalizada  en testing
  gan  <-  fmetrica_ganancia_rpart_prob(testing_prediccion$predictions[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], ppcorte, pclase_valor_positivo, pproblema)
  # calculo el AUC en testing
  auc  <-  fmetrica_auc_rpart(testing_prediccion$predictions[, pclase_valor_positivo ],  ptest[, get(pclase_nomcampo)], pclase_valor_positivo )


  return( list("ganancia_test"=gan, "auc_test"=auc) )
}

#------------------------------------------------------------------------------

modelo_ranger_ganancia_MBO_baja12 = function(x = list(pnum.trees, pmin.node.size, pmtry, psplitrule, pmeses_cantidad, ppcorte ) )
{
  # x$psplitrule vale 0 o 1
  translator_vector <- c( "gini",  "extratrees"  )
  vsplitrule        <-  translator_vector[ x$psplitrule ] 

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )

  vds$actual_train[ , (env$data$clase_nomcampo) := as.factor( ifelse( get(env$data$clase_nomcampo) %in% env$data$clases_juntar, 'BAJA+2', 'CONTINUA' ) ) ] 
  vds$futuro_train[ , (env$data$clase_nomcampo) := as.factor( ifelse( get(env$data$clase_nomcampo) %in% env$data$clases_juntar, 'BAJA+2', 'CONTINUA' ) ) ]
 
  #quito los nulos
  vds$actual_train    <-  na.roughfix( vds$actual_train )
  vds$futuro_train    <-  na.roughfix( vds$futuro_train )
  dataset$actual_test <-  na.roughfix( dataset$actual_test )
  dataset$futuro_test <-  na.roughfix( dataset$futuro_test )

  mactual  <-  modelo_ranger_pcorte( vds$actual_train,  dataset$actual_test, 
                                     pclase_nomcampo=env$data$clase_nomcampo, pclase_valor_positivo=env$data$clase_valor_positivo, 
                                     pproblema=env$problema, env$montecarlo$semilla_azar[1],
                                     pnum.trees=x$pnum.trees,  pmin.node.size=x$pmin.node.size, pmtry=x$pmtry, psplitrule=vsplitrule, ppcorte=x$ppcorte )

  mfuturo  <-  modelo_ranger_pcorte( vds$futuro_train,  dataset$futuro_test, 
                                     pclase_nomcampo=env$data$clase_nomcampo, pclase_valor_positivo=env$data$clase_valor_positivo, 
                                     pproblema=env$problema, env$montecarlo$semilla_azar[1],
                                     pnum.trees=x$pnum.trees,  pmin.node.size=x$pmin.node.size, pmtry=x$pmtry, psplitrule=vsplitrule, ppcorte=x$ppcorte )

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("num.trees=",     x$pnum.trees, ", " ,
                        "min.node.size=", x$pmin.node.size,  ", " ,
                        "mtry=",          x$pmtry,  ", " ,
                        "splitrule=",     vsplitrule, ", " , 
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
                          pclase="ternaria"
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  #borro los datasets intermedios creados
  gc()

  return(- mean(mactual$ganancia_test))   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

modelo_ranger_ganancia_MBO_meses1 = function(x = list(pnum.trees, pmin.node.size, pmtry, psplitrule, pmeses_cantidad, psample ) )
{

  # x$psplitrule vale 1 o 2
  translator_vector <- c( "gini",  "extratrees"  )
  vsplitrule        <-  translator_vector[ x$psplitrule ] 

  t0   <-  Sys.time()
  #aqui se debe usar un psample adecuado
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,  x$pmeses_cantidad, x$psample )

  mactual  <-  modelo_ranger_pcorte( vds$actual_train,  dataset$actual_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pnum.trees=x$pnum.trees, pmin.node.size=x$pmin.node.size, pmtry=x$pmtry, psplitrule=vsplitrule, psemilla=17, ppcorte=0.025 )
  mfuturo  <-  modelo_ranger_pcorte( vds$futuro_train,  dataset$futuro_test, env$data$clase_nomcampo, env$data$clase_valor_positivo, env$problema, pnum.trees=x$pnum.trees, pmin.node.size=x$pmin.node.size, pmtry=x$pmtry, psplitrule=vsplitrule, psemilla=17, ppcorte=0.025 )

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste("num.trees=",      x$pnum.trees, ", " ,
                        "min.node.size=",  x$pmin.node.size, ", " ,
                        "mtry=",           x$pmtry,  ", " ,
                        "splitrule=",      vsplitrule, ", " ,
                        "psample=",        x$psample, 
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
