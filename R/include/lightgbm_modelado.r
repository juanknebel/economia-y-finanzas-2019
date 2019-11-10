
vprob_corte_binaria2  <- 0.025

#------------------------------------------------------------------------------

fganancia_logistic_lightgbm_binaria2   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > vprob_corte_binaria2  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto ),
                na.rm = TRUE   
            )
        

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan) , 0, gan ) ,
                  higher_better= TRUE 
                )
         )
}

#------------------------------------------------------------------------------

modelo_lightgbm_pcorte = function( ptrain, ptest, lgbm, psemilla, pnum_iterations, pfeature_fraction, plearning_rate, pmin_data_in_leaf, pnum_leaves, plambda_l1, pmin_gain_to_split, ppcorte   )
{

  #modifico una  variable GLOBAL
  vprob_corte_binaria2 <<- ppcorte

  modelo = lgb.train(
                     seed = psemilla, 
                     data = ptrain,  
                     valids= list( valid=ptest),
                     objective="binary",
	             eval = fganancia_logistic_lightgbm_binaria2 ,
                     metric = "auc",
                     num_iterations= pnum_iterations,
                     boost_from_average=TRUE,
                     bagging_fraction = lgbm$bagging_fraction, 
                     feature_fraction = pfeature_fraction, 
                     learning_rate = plearning_rate,
                     min_data_in_leaf = pmin_data_in_leaf, 
                     num_leaves = pnum_leaves,
                     lambda_l1 = plambda_l1, min_gain_to_split = pmin_gain_to_split,
                     max_bin = lgbm$max_bin,
                     num_threads = 16
                    )

  
  #esta es la forma NATURAL  de calcular la ganancia
  gan_ultima     <- unlist( modelo$record_evals$valid$ganancia$eval )[ pnum_iterations ] 
  auc_ultima     <- unlist( modelo$record_evals$valid$auc$eval )[ pnum_iterations ] 
  

  return( list("ganancia_test"=gan_ultima, "auc_test"=auc_ultima ) )
}

#------------------------------------------------------------------------------

formato_lightgbm = function(  pdataset  )
{
  
  dataset_clase  <- as.numeric(  pdataset[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  ) 
  
  dataset_sinclase   <- pdataset[ , ! env$data$clase_nomcampo, with=FALSE   ]

  #genero el formato requerido por XGBoost
  ddataset  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase),
                              label = dataset_clase, 
                              free_raw_data=FALSE 
                            )

  return(  ddataset )
}
#------------------------------------------------------------------------------

formato_lightgbm_BAJA12 = function(  pdataset  )
{
  
  dataset_clase  <- as.numeric(  pdataset[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivos  ) 
  
  dataset_sinclase   <- pdataset[ , ! ( env$data$clase_nomcampo), with=FALSE   ]

  #genero el formato requerido por XGBoost
  ddataset  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase),
                              label = dataset_clase, 
                              free_raw_data=FALSE 
                            )

  return(  ddataset )
}
#------------------------------------------------------------------------------
                    
modelo_lightgbm_ganancia_MBO_baja12 = function(x = list(pnum_iterations, pfeature_fraction, plearning_rate, plambda_l1, pmin_gain_to_split, pmin_data_in_leaf, pnum_leaves, ppcorte, pmeses_cantidad ) )
{

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )


  dactual_train <-  formato_lightgbm_BAJA12( vds$actual_train )
  dfuturo_train <-  formato_lightgbm_BAJA12( vds$futuro_train )

  dactual_test  <-  formato_lightgbm( dataset$actual_test ) 
  dfuturo_test  <-  formato_lightgbm( dataset$futuro_test ) 

  mactual  <-  modelo_lightgbm_pcorte( dactual_train,  dactual_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations,
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )

  mfuturo  <-  modelo_lightgbm_pcorte( dfuturo_train,  dfuturo_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations, 
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )
 

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste( "num_iterations=",     x$pnum_iterations,       ", ",
                         "bagging_fraction=",   lgbm$bagging_fraction,   ", ",
                         "feature_fraction=",   x$pfeature_fraction, ", ",
                         "learning_rate=",      x$plearning_rate,    ", ",
                         "min_data_in_leaf=",   x$pmin_data_in_leaf, ", ",
                         "num_leaves=",         x$pnum_leaves,       ", ",
                         "lambda_l1=",          x$plambda_l1,        ", ",
                         "min_gain_to_split=",  x$pmin_gain_to_split,", ",
                         "max_bin=",            lgbm$max_bin,             
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
                          pclase="BAJA12"
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  ganancia_test <- mactual$ganancia_test

  #borro los datasets intermedios creados
  rm( dactual_train, dactual_test, dfuturo_train, dfuturo_test  )
  gc()

  
  return(  - ganancia_test )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

modelo_lightgbm_auc_MBO_baja12 = function(x = list(pnum_iterations, pfeature_fraction, plearning_rate, plambda_l1, pmin_gain_to_split, pmin_data_in_leaf, pnum_leaves, ppcorte, pmeses_cantidad ) )
{

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )


  dactual_train <-  formato_lightgbm_BAJA12( vds$actual_train )
  dfuturo_train <-  formato_lightgbm_BAJA12( vds$futuro_train )

  dactual_test  <-  formato_lightgbm( dataset$actual_test ) 
  dfuturo_test  <-  formato_lightgbm( dataset$futuro_test ) 


  mactual  <-  modelo_lightgbm_pcorte( dactual_train,  dactual_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations,
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )

  mfuturo  <-  modelo_lightgbm_pcorte( dfuturo_train,  dfuturo_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations, 
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )
 

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste( "num_iterations=",     x$pnum_iterations,       ", ",
                         "bagging_fraction=",   lgbm$bagging_fraction,   ", ",
                         "feature_fraction=",   x$pfeature_fraction, ", ",
                         "learning_rate=",      x$plearning_rate,    ", ",
                         "min_data_in_leaf=",   x$pmin_data_in_leaf, ", ",
                         "num_leaves=",         x$pnum_leaves,       ", ",
                         "lambda_l1=",          x$plambda_l1,        ", ",
                         "min_gain_to_split=",  x$pmin_gain_to_split,", ",
                         "max_bin=",            lgbm$max_bin,             
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
                          pclase="BAJA12"
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  auc_test <- mactual$auc_test

  #borro los datasets intermedios creados
  rm( dactual_train, dactual_test, dfuturo_train, dfuturo_test  )
  gc()

  
  return(  - auc_test )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------

modelo_lightgbm_auc_MBO_baja = function(x = list(pnum_iterations, pfeature_fraction, plearning_rate, plambda_l1, pmin_gain_to_split, pmin_data_in_leaf, pnum_leaves, ppcorte, pmeses_cantidad ) )
{

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )


  dactual_train <-  formato_lightgbm( vds$actual_train )
  dfuturo_train <-  formato_lightgbm( vds$futuro_train )

  dactual_test  <-  formato_lightgbm( dataset$actual_test ) 
  dfuturo_test  <-  formato_lightgbm( dataset$futuro_test ) 


  vinit_score_actual  <- mean(getinfo(dactual_train, "label"))

  mactual  <-  modelo_lightgbm_pcorte( dactual_train,  dactual_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations,
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )


  mfuturo  <-  modelo_lightgbm_pcorte( dfuturo_train,  dfuturo_test, 
                                       env$lgbm, env$montecarlo$semilla_azar[1],
                                       pnum_iterations=x$pnum_iterations, 
                                       pfeature_fraction=x$pfeature_fraction, 
                                       plearning_rate= x$plearning_rate, 
                                       plambda_l1=x$plambda_l1, 
                                       pmin_gain_to_split=x$pmin_gain_to_split, 
                                       pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                       pnum_leaves=x$pnum_leaves,
                                       ppcorte=x$ppcorte )
 

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste( "num_iterations=",     x$pnum_iterations,       ", ",
                         "bagging_fraction=",   lgbm$bagging_fraction,   ", ",
                         "feature_fraction=",   x$pfeature_fraction, ", ",
                         "learning_rate=",      x$plearning_rate,    ", ",
                         "min_data_in_leaf=",   x$pmin_data_in_leaf, ", ",
                         "num_leaves=",         x$pnum_leaves,       ", ",
                         "lambda_l1=",          x$plambda_l1,        ", ",
                         "min_gain_to_split=",  x$pmin_gain_to_split,", ",
                         "max_bin=",            lgbm$max_bin,             
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
                          parchivo_futuro_test= paste( env$data$ldata$mes_futuro_test,  env$data$ldata$mes_sufijo, sep="")
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  auc_test <- mactual$auc_test

  #borro los datasets intermedios creados
  rm( dactual_train, dactual_test, dfuturo_train, dfuturo_test  )
  gc()

  
  return(  - auc_test )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------
