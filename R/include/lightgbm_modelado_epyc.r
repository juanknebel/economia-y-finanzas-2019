#esta variable global se asigna adentro de fganancia_logistic_epyc
vpcorte_global_idx <- 1
vpcorte_global     <-  rep( 0.025, 10000 )

#------------------------------------------------------------------------------

fbuscar_pcorte_epyc   <- function(probs, clases) 
{

   unidos <-  cbind( probs, runif(length(clases)), clases )
   unidos <- as.data.table( unidos )
   names( unidos ) <- c( "prob", "azar", "clase" )

   unidos <- unidos[ order( -probs, azar),   ]
  
   gan_acum <-  cumsum( ifelse( unidos[ ,"clase"]== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto ) )
   
   pos <-  which.max( gan_acum )


   return(  unidos$prob[ pos ] )
}

#------------------------------------------------------------------------------


fganancia_logistic_actual_epyc   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")

   unidos <-  cbind( probs, runif(length(vlabels)), vlabels )
   unidos <- as.data.table( unidos )
   names( unidos ) <- c( "prob", "azar", "clase" )

   unidos <- unidos[ order( -probs, azar),   ]
  
   gan_acum <-  cumsum( ifelse( unidos[ ,"clase"]== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto ) )
   
   pos <-  which.max( gan_acum )

   gan_max  <- gan_acum[ pos ]  


   vpcorte_global[ vpcorte_global_idx ]  <<-  unidos$prob[ pos ] 
   vpcorte_global_idx <<-  vpcorte_global_idx + 1

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan_max) , 0, gan_max ) ,
                  higher_better= TRUE 
                )
         )
}

#------------------------------------------------------------------------------
#genero un modelo estimando num_iterations y pcorte
#ptest siempre es un mes entero


#ptrain<- dfuturo$train
#pvalidate<- dfuturo$validate
#ptest<- dfuturo$test

modelo_lightgbm_aplicar = function( ptrain, pvalidate_sinclase, pvalidate_clase, ptest_sinclase, ptest_clase,
                                    lgbm, pnum_iterations, pfeature_fraction, plearning_rate, pmin_data_in_leaf, pnum_leaves, plambda_l1, pmin_gain_to_split )
{

  #genero el modelo SIN estimar ninguna ganancia
  modelo = lgb.train(
                     seed = lgbm$semilla, 
                     data = ptrain,  
                     objective="binary",
                     metric = "auc",
                     num_iterations= pnum_iterations,

                     boost_from_average=TRUE,
                     bagging_fraction = lgbm$bagging_fraction, 
                     feature_fraction = pfeature_fraction, 
                     learning_rate = plearning_rate,
                     min_data_in_leaf = pmin_data_in_leaf, 
                     num_leaves = pnum_leaves,
                     lambda_l1 = plambda_l1, min_gain_to_split = pmin_gain_to_split,
                     max_bin = lgbm$max_bin
                    )



  validacion_prediccion  <- predict(  modelo, as.matrix(pvalidate_sinclase) )
  pcorte <- fbuscar_pcorte_epyc( validacion_prediccion, pvalidate_clase )


  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, as.matrix(ptest_sinclase) )

  #calculo AUC
  pred             <-  ROCR::prediction(  aplicacion_prediccion, ptest_clase, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
  auc_test <-  unlist(auc_testing@y.values)


  ganancia_test <-  fmetrica_ganancia_lightgbm( pcorte, aplicacion_prediccion,  ptest_clase, env$problema ) 

  return( list("ganancia_test"=ganancia_test, "auc_test"=auc_test, "pcorte"=pcorte ) )
}

#------------------------------------------------------------------------------
#genero un modelo estimando num_iterations y pcorte
#ptest siempre es un mes entero

#ptrain<- dactual$train
#pvalidate<- dactual$validate
#ptest<- dactual$test
#lgbm<-env$lgbm
#pfeature_fraction<-x$pfeature_fraction
#plearning_rate<-x$plearning_rate
#pmin_data_in_leaf<-x$pmin_data_in_leaf
#pnum_leaves<-x$pnum_leaves
#plambda_l1<-x$plambda_l1
#pmin_gain_to_split<-x$pmin_gain_to_split

modelo_lightgbm_estimar = function( ptrain, pvalidate, ptest_sinclase, ptest_clase, lgbm, pfeature_fraction, plearning_rate, pmin_data_in_leaf, pnum_leaves, plambda_l1, pmin_gain_to_split )
{

  vpcorte_global_idx <<- 1

  modelo = lgb.train(
                     seed = lgbm$semilla, 
                     data = ptrain,  
                     valids= list( valid= pvalidate),
                     eval = fganancia_logistic_actual_epyc,
                     objective="binary",
                     metric = "auc",
                     num_iterations= lgbm$num_iterations_tope,

                     early_stopping_round = lgbm$early_stopping_round,
                     boost_from_average=TRUE,
                     bagging_fraction = lgbm$bagging_fraction, 
                     feature_fraction = pfeature_fraction, 
                     learning_rate = plearning_rate,
                     min_data_in_leaf = pmin_data_in_leaf, 
                     num_leaves = pnum_leaves,
                     lambda_l1 = plambda_l1, min_gain_to_split = pmin_gain_to_split,
                     max_bin = lgbm$max_bin
                    )


  largo <-  length( unlist( modelo$record_evals$valid$ganancia$eval ) )

  iteracion_max  <- which.max(  unlist( modelo$record_evals$valid$ganancia$eval ) ) 


  ganancia_validate <-  unlist( modelo$record_evals$valid$ganancia$eval)[ iteracion_max ]
  ganancia_validate <-  ganancia_validate * ( 1.0 /  env$data$actual_training_prob )
  
  #esta es la forma NATURAL  de calcular la ganancia
  auc_validate     <- unlist( modelo$record_evals$valid$auc$eval )[ iteracion_max ] 

  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, as.matrix(ptest_sinclase) )

  #calculo AUC
  pred             <-  ROCR::prediction(  aplicacion_prediccion, ptest_clase, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
  auc_test <-  unlist(auc_testing@y.values)

  pcorte <-  vpcorte_global[ iteracion_max ]
  ganancia_test <-  fmetrica_ganancia_lightgbm( pcorte, aplicacion_prediccion,  ptest_clase, env$problema ) 
  ganancia_test <-  ganancia_test * ( 1.0 /  env$data$actual_training_prob )

  return( list("ganancia_test"=ganancia_test, "auc_test"=auc_test, "pcorte"=pcorte, "num_iterations"=iteracion_max ) )
}

#------------------------------------------------------------------------------

formato_lightgbm_futuro =  function(  ptrain, ptest, pclase )
{

  #Divido ptest en 90% /10% , usando la libreria dplyr
  vtrain  <- as.data.table(mutate(ptrain, idtempo =  row_number()))

  set.seed( env$montecarlo$semilla_azar[1] )
  vtrain1  <- as.data.table(vtrain %>%
  group_by(!!as.name(env$data$clase_nomcampo)) %>%
  sample_frac(env$data$futuro_training_prob) %>%
  ungroup)
  vtrain2  <- as.data.table(anti_join(vtrain, vtrain1, by = "idtempo"))

  vtrain1[ ,  idtempo := NULL    ] 
  vtrain2[ ,  idtempo := NULL    ]  

  
  #clase de train1
  if(  pclase== 1 )  {
    #Solo los BAJA+2 son positivos
    vtrain1[ , (env$data$clase_nomcampo) := as.numeric(  vtrain1[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  ) ]
  } else {
    #Los BAJA+2 y  BAJA+2  son positivos
    vtrain1[ , (env$data$clase_nomcampo) := as.numeric(  vtrain1[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivos  ) ]
  }

  #ptrain2  solo los BAJA+2  son positivos
  vtrain2[ , (env$data$clase_nomcampo) := as.numeric(  vtrain2[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  ) ]
  
  vtrain2_clase      <-  vtrain2[ , get(env$data$clase_nomcampo) ]
  vtrain2_sinclase   <-  vtrain2[ , ! ( env$data$clase_nomcampo), with=FALSE   ]


  #paso ptrain1 a formato lightgbm

  vtrain1_clase     <-  vtrain1[ , get(env$data$clase_nomcampo) ]
  vtrain1_sinclase  <-  vtrain1[ , ! ( env$data$clase_nomcampo), with=FALSE   ]


  #genero el formato requerido por LightGBM
  vtrain1_lgbm  <-   lgb.Dataset( data  = data.matrix(vtrain1_sinclase),
                                  label = vtrain1_clase, 
                                  free_raw_data=FALSE 
                                )



  #paso la clase a 0 ,1   atencion que solamente los BAJA+2  son  1
  vtest <- ptest
  vtest_clase     <-  as.numeric(  vtest[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  ) 
  vtest_sinclase  <-  vtest[ , ! ( env$data$clase_nomcampo), with=FALSE   ]

 
  return(  list( "train"=vtrain1_lgbm ,  
                  "validate_sinclase"=vtrain2_sinclase,  "validate_clase"=vtrain2_clase,  
                  "test_sinclase"=vtest_sinclase,  "test_clase"=vtest_clase
               ) )
}
#------------------------------------------------------------------------------

#ptrain <- vds$actual_train
#ptest  <- dataset$actual_test
#pclase <- x$pclase

formato_lightgbm_actual =  function(  ptrain, ptest, pclase )
{
  
 
  set.seed( env$montecarlo$semilla_azar[1] )
  ptrain[, azar := runif( nrow(ptrain) ) ]

  vtrain <- ptrain[ (get(env$data$clase_nomcampo) == (env$data$actual_clase_subsampling) & azar <= env$data$actual_subsampling) |  (get(env$data$clase_nomcampo) != (data$actual_clase_subsampling) ),  ] 

  vtrain[, "azar" := NULL] 


  
  #clase de train
  if(  pclase== 1 )  {
    #Solo los BAJA+2 son positivos
    vtrain_clase <- as.numeric(  vtrain[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  )
  } else {
    #Los BAJA+2 y  BAJA+2  son positivos
    vtrain_clase <- as.numeric(  vtrain[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivos  )
  }


  vtrain_sinclase  <-  vtrain[ , ! ( env$data$clase_nomcampo), with=FALSE   ]


  #genero el formato requerido por LightGBM
  vtrain_lgbm  <-   lgb.Dataset( data  = data.matrix(vtrain_sinclase),
                                 label = vtrain_clase, 
                                 free_raw_data=FALSE 
                               )


  #division de test

  #Divido ptest en 50% /50% , usando la libreria dplyr
  vtest  <- as.data.table(mutate(ptest, idtempo =  row_number()))
  #paso la clase a 0 ,1   atencion que solamente los BAJA+2  son  1
  vtest[ , (env$data$clase_nomcampo) := as.numeric(  vtest[ ,get(env$data$clase_nomcampo)] %in%  env$data$clase_valor_positivo  ) ]


  set.seed( env$montecarlo$semilla_azar[1] )
  vtest1  <- as.data.table(vtest %>%
  group_by(!!as.name(env$data$clase_nomcampo)) %>%
  sample_frac(env$data$actual_training_prob) %>%
  ungroup)
  vtest2  <- as.data.table(anti_join(vtest, vtest1, by = "idtempo"))

  vtest1[ ,  idtempo := NULL    ] 
  vtest2[ ,  idtempo := NULL    ] 

  #vtest2 viaja directo, pero vtest1 lo debo pasar a formato lightgbm
  vtest2_clase     <-  vtest2[ , get(env$data$clase_nomcampo) ]
  vtest2_sinclase  <-  vtest2[  , ! ( env$data$clase_nomcampo), with=FALSE  ]


  vtest1_clase      <-  vtest1[ , get(env$data$clase_nomcampo) ]
  vtest1_sinclase   <-  vtest1[ , ! ( env$data$clase_nomcampo), with=FALSE   ]

  #genero el formato requerido por LightGBM
  vtest1_lgbm  <-   lgb.Dataset( data  = data.matrix(vtest1_sinclase),
                                 label = vtest1_clase, 
                                 free_raw_data=FALSE 
                               )

 
  return(  list( "train"=vtrain_lgbm ,  "validate"=vtest1_lgbm ,  "test_sinclase"=vtest2_sinclase, "test_clase"=vtest2_clase ) )
}
#------------------------------------------------------------------------------

x <- list(  "pfeature_fraction"=0.3502501, "plearning_rate"=0.01457441, "plambda_l1"=0.5221028, 
            "pmin_gain_to_split"=0.2479507, "pmin_data_in_leaf"=56, "pnum_leaves"=745,
            "pclase"=1, "pmeses_cantidad"=9 )

modelo_lightgbm_epyc = function(x = list(pfeature_fraction, plearning_rate, plambda_l1, pmin_gain_to_split, pmin_data_in_leaf, pnum_leaves, pclase, pmeses_cantidad ) )
{

  t0   <-  Sys.time()
  vds  <-  dataset_generar_meses( dataset$completo,  env$data,    x$pmeses_cantidad )
 
  dactual <-  formato_lightgbm_actual(  vds$actual_train, dataset$actual_test, x$pclase )
  dfuturo <-  formato_lightgbm_futuro(  vds$futuro_train, dataset$futuro_test, x$pclase )

  #Genero el modelo con los datos actuales
  # num_iterations y pcorte optimos se estiman aqui, el resto de los parametros vino fijo

  mactual  <-  modelo_lightgbm_estimar( dactual$train, dactual$validate, dactual$test_sinclase, dactual$test_clase,
                                        env$lgbm,
                                        pfeature_fraction=x$pfeature_fraction, 
                                        plearning_rate= x$plearning_rate, 
                                        plambda_l1=x$plambda_l1, 
                                        pmin_gain_to_split=x$pmin_gain_to_split, 
                                        pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                        pnum_leaves=x$pnum_leaves )

 
  #Vuelvo a generar y aplicar el modelo con los datos del futuro, meses distintos
  #aqui todos los parametros llegan fijos en particular tambien num_iterations y pcorte

  mfuturo  <-  modelo_lightgbm_aplicar( dfuturo$train,  
                                        dfuturo$validate_sinclase, dfuturo$validate_clase,
                                        dfuturo$test_sinclase, dfuturo$test_clase,
                                        env$lgbm,
                                        pnum_iterations=mactual$num_iterations, 
                                        pfeature_fraction=x$pfeature_fraction, 
                                        plearning_rate= x$plearning_rate, 
                                        plambda_l1=x$plambda_l1, 
                                        pmin_gain_to_split=x$pmin_gain_to_split, 
                                        pmin_data_in_leaf=x$pmin_data_in_leaf, 
                                        pnum_leaves=x$pnum_leaves ) 

  
  t1   <-  Sys.time()
  tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")


  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste( "num_iterations=",     mactual$num_iterations,       ", ",
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
                          pprob_corte= mfuturo$pcorte, 
                          pst_parametros= st_parametros, 
                          parchivo_actual_train= vds$actual_train_st,
                          parchivo_actual_test= paste( env$data$ldata$mes_actual_test,  env$data$ldata$mes_sufijo, sep=""), 
                          parchivo_futuro_train= vds$futuro_train_st, 
                          parchivo_futuro_test= paste( env$data$ldata$mes_futuro_test,  env$data$ldata$mes_sufijo, sep=""),
                          pclase= ifelse( x$pclase==1, "BINARIA1", "BINARIA2" )
                        )


  #escribo el grafico con la evolucion de la optimizacion
  hiperparametros_refrescar( env$hiper )

  auc_test       <- mactual$auc_test
  ganancia_test  <- mactual$ganancia_test

  #borro los datasets intermedios creados
  #rm( dactual$train, dactual$validate, dactual$test, dfuturo$train,  dfuturo$validate, dfuturo$test )
  gc()

  
  return(  - ganancia_test )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------------------------------
