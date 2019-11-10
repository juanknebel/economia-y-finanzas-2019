library("data.table")
library("lightgbm")


#cargo los datasets
setwd( "~/cloud/cloud1/datasets/dias/")
dataset_train <- fread( "201902_dias.txt" )
dataset_test  <- fread( "201904_dias.txt" )


nrow( dataset_train )
nrow( dataset_test )


#Agrego los canaritos a ambos datasets
kcanaritos_cantidad <- 20
vcanaritos <-  paste0( "canarito", 1:kcanaritos_cantidad )

#uso esta semilla para los canaritos
set.seed(102171)

dataset_train[ , (vcanaritos) := 0 ]
dataset_train[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]
dataset_test[  , (vcanaritos) := 0 ]
dataset_test[  , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]

#ahora hago que los canaritos sean las primeras variables del dataset
nuevo_orden <-  c( vcanaritos, setdiff( colnames( dataset_train), vcanaritos )) 

setcolorder( dataset_train, nuevo_orden )
setcolorder( dataset_test,  nuevo_orden )

#verifico que los canaritos esten al comienzo del dataset
colnames( dataset_train )[1:40]


#------------------------------


#LightGBM solo trabaja con clase binaria
#dejo la clase en {0,1} 
dataset_train[  , clase_ternaria := as.integer(clase_ternaria == "BAJA+2" ) ]
dataset_test[   , clase_ternaria := as.integer(clase_ternaria == "BAJA+2" ) ]



#genero el formato requerido por LightGBM
dtrain  <-   lgb.Dataset( data  = data.matrix(dataset_train[ , !"clase_ternaria", with=FALSE ]),
                          label = dataset_train[, clase_ternaria], 
                          free_raw_data=FALSE )

dtest  <-    lgb.Dataset( data  = data.matrix(dataset_test[ , !"clase_ternaria", with=FALSE ]),
                          label = dataset_test[, clase_ternaria], 
                          free_raw_data=FALSE )



#Genero el modelo con LightGBM
m1 = lgb.train( data= dtrain,
                objective= "binary",
                valid= c( test=dtest ),
                is_training_metric=TRUE,
                eval= "auc",
                num_iterations= 1000,
                early_stopping_rounds= 20,
                num_leaves= 1024,
                verbose= 2
              )

setwd( "~/cloud/cloud1/work/" )
m1$save_model("modelo1.txt")
 
#Se entrenaron 136 arboles
#El mejor auc fue de test's auc:0.91194

#importancia de las variables
lgb.importance( m1 )[ 1:30]



#aparecen muchos canaritos !



#Ahora uso el "nuevo parametro del LightGBM"


#Genero el modelo
m2 = lgb.train( data= dtrain,
                objective= "binary",
                valid= c( test=dtest ),
                is_training_metric=TRUE,
                eval= "auc",
                num_iterations= 1000,
                early_stopping_rounds= 20,
                num_leaves= 1024,
                predict_leaf_index= TRUE,
                verbose= 2,
                num_canaritos=kcanaritos_cantidad
              )


setwd( "~/cloud/cloud1/work/" )
m2$save_model("modelo2.txt")


#El arbol 57 fue el ultimo que se pudo crear
#A partir del arbol 58, como el mejor corte era Canarito
#ya dejo de cortar


#El mejor auc fue de  test's auc:0.921841
#sin los canaritos habia dado auc:0.906394
#mejoramos !


#importancia de las variables
lgb.importance( m2 )[ 1:30]



#No hay canaritos !


