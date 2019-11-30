#comparacion de lineas de muerte con undersampling y SIN undersampling

#limpio la memoria
rm(list=ls())
gc()

library( "data.table" )
library( "xgboost" )

#cargo los datasets
setwd( "~/cloud/cloud1/datasets/")
dataset   <-   fread( "paquete_premium_littlerobots.txt.gz" )


#dejo la clase en 0,1
#dataset[  , clase01 := as.integer(clase_ternaria=="BAJA+2") ]
dataset[  , clase01 := clase_ternaria ]

undersampling <- 1.0
dataset[  , sample := runif( nrow(dataset) ) ]


mes_aplicacion <- c( 201904, 201903, 201902, 201901, 201812, 201811, 201810, 201809, 201808, 201807, 201806 )
mes_desde      <- c( 201805, 201804, 201803, 201802, 201801, 201712, 201711, 201710, 201709, 201708, 201707 )
mes_hasta      <- c( 201902, 201901, 201812, 201811, 201810, 201809, 201808, 201807, 201806, 201805, 201804 )


for( i in 1:length(mes_aplicacion) )
{

  dataset[  , train:=0 ]
  dataset[  , train:= (clase01==1 |  sample< undersampling ) & foto_mes>=mes_desde[i] & foto_mes<=mes_hasta[i] ]
  
  dgeneracion  <-   xgb.DMatrix( data  = data.matrix( dataset[ train==1 , !c("numero_de_cliente","clase_ternaria","clase01","sample","train"), with=FALSE]),
                                 label = dataset[ train==1, clase01 ]
                               )

  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles

  modelo = xgb.train( 
                      data= dgeneracion,
                      objective= "binary:logistic",
                      #tree_method= "hist",
                      max_bin= 31,  
                      base_score= mean( getinfo(dgeneracion, "label") ),
                      eta= 0.04,
                      nrounds= 300, 
                      colsample_bytree= 0.6,
                      verbose= -1
                    )

  daplicacion  <-   xgb.DMatrix( data  = data.matrix( dataset[ foto_mes== mes_aplicacion[i], !c("numero_de_cliente","clase_ternaria","clase01","sample","train"), with=FALSE]),
                                 label = dataset[ foto_mes==mes_aplicacion[i], clase01 ]
                               )
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )

  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <-  cbind(  dataset[ foto_mes==mes_aplicacion[i], c("numero_de_cliente","clase01") ], aplicacion_prediccion )

  #le doy nombre a las columnas
  colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )

  prob_corte           <-   500*(1/undersampling)/( 19500 + 500*(1/undersampling) )
  #para calcular la ganancia, cuando se corre para meses del pasado
  ganancia_undersampling <- sum(  prediccion_final[ prob_positivo> prob_corte,   ifelse( clase01 == 1  , 19500, -500) ] )

  cat( mes_aplicacion[i],  ganancia_undersampling, "\n" )

}