#Este programa genera la salida final
#La salida queda en  /work/  lineademuerte_entregar_UBA.txt  , lineademuerte_probabilidades_UBA.txt  y lineademuerte_importancia_UBA.txt
#Este programa necesita para correr 8vCPU y 128GB de RAM
#para correr se merece una digna maquina Inmortal
#Correrlo desde RStudio

#Atencion, se trabaja con el archivo  _hist 
#No se crea ninguna variable en el mismo mes
#No se imputan nulos, no se quitan outliers
#No se crean variables que sean predicciones de modelos
#No se hace undersampling, se trabaja con todos los datos (y por eso necesita 128GB de RAM)
#No se optimiza la probabilidad de corte, se deja en la original de 0.025
#No se trabaja con clase_binaria2  positivos = { BAJA+1, BAJA+2 }
#No se ajusta por inflacion
#No se traen variables externas como el valor mensual del dolar
#No se hace stacking de modelos al final

#limpio la memoria
rm(list=ls())
gc()

library( "data.table" )
library( "lightgbm" )

#aplico el modelo a junio, que NO tiene clase
#la decision de entrenar en 10 meses se obtuvo de los testeos hechos sobre junio del 2018
#debe tenerse en cuenta que para 201906 la clase esta VACIA
#por lo que NO se puede calcular la ganancia
#esta vez, realmente estamos prediciendo el futuro
#es un salto al vacio

#cargo los datasets
setwd( "~/cloud/cloud1/datasets/")
dataset   <-   fread( "paquete_premium_hist.txt.gz" )
nombre_archivo = "primer_prueba_lightgbm"

#dejo la clase en 0,1
dataset[  , clase_ternaria := as.integer(clase_ternaria=="BAJA+2") ]

#entreno en 10 meses,  periodo  [ 201807, 201904 ]
degeneracion <- lgb.Dataset( data  = data.matrix(dataset[ foto_mes>=201807 & foto_mes<=201904, !c("numero_de_cliente","clase_ternaria") , with=FALSE]),
                             label = dataset[foto_mes>=201807 & foto_mes<=201904, clase_ternaria], 
                             free_raw_data=FALSE 
)

#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed( 209809 ) #mi querida random seed, para que las corridas sean reproducibles

# ventana=12, num_iterations=46, learning_rate=0.136955288489042, lambda_l1=1.91098677561894,
#  lambda_l2=17.8666833393839, min_gain_to_split=4.04465876323807, min_data_in_leaf=37, max_depth=20, 
# feature_fraction=0.731661600525934, max_bin=255, subsample=1
modelo = lgb.train( 
  data = degeneracion,
  objective = "binary",
  metric="auc",
  seed= 209809,
  num_iterations=46, 
  boost_from_average=FALSE,
  bagging_fraction=1, 
  feature_fraction=0.73, 
  learning_rate=0.13, 
  min_child_weight=8, 
  max_depth=20, 
  lambda_l1=1.91,
  lambda_l2=17.86,
  max_bin=255, 
  num_leaves=255
)

#aplico a los datos de 201906, que tienen la clase vacia
daplicacion  <-  dataset[ foto_mes==201906, !c("numero_de_cliente","clase_ternaria")]
                                       
#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, as.matrix(daplicacion ))

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <-  cbind(  dataset[ foto_mes==201906, c("numero_de_cliente","clase_ternaria") ], aplicacion_prediccion )

#le doy nombre a las columnas
colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )

#para calcular la ganancia, cuando se corre para meses del pasado
#sum(  prediccion_final[ prob_positivo>0.025,   ifelse( clase01 == 1  , 19500, -500) ] )

#Genero las TRES salidas
#grabo todas las probabilidad, simplemente para tenerlo
setwd(  "~/cloud/cloud1/work/")
fwrite( prediccion_final[ order( -prob_positivo) ], 
        file=paste0(nombre_archivo,"_probabilidades.txt"), 
        sep="\t", 
        eol = "\r\n")

#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ] ), 
        file=paste0(nombre_archivo,"_entregar.txt"), 
        col.names=FALSE, 
        sep="\t", 
        eol = "\r\n")

#grabo la importancia de las variables
write.table(  lgb.importance( model = modelo ),
              file=paste0(nombre_archivo,"_importancia.txt"), 
              , sep="\t"
              , eol = "\r\n"
)
