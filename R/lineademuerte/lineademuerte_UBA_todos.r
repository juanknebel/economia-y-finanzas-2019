#Este programa calcua la ganancia para cada mes del pasado, con el modelo de la línea de muerte
#La salida queda en  /work/  lineademuerte_entregar_UBA.txt  , lineademuerte_probabilidades_UBA.txt  y lineademuerte_importancia_UBA.txt
#Este programa necesita para correr 8vCPU y 128GB de RAM
#para correr se merece una digna maquina Inmortal
#Correrlo desde RStudio

#limpio la memoria
rm(list=ls())
gc()

library( "data.table" )
library( "xgboost" )

undersampling = 1

#___________________________________________________________
# Función para calcular la línea de muerte de todos los meses
linea_muerte <- function( pmes_cero )
{

  # Los meses en su código de "mes"
  futuro = as.numeric(tb_meses[foto_mes == pmes_cero, mes])
  fin = as.numeric(futuro+2)
  inicio = as.numeric(fin+9)
  
  
  # Los meses en su forma foto_mes
  primero = as.numeric(tb_meses[mes == inicio, foto_mes])
  ultimo = as.numeric(tb_meses[mes == fin, foto_mes])
  pred = as.numeric(pmes_cero)
  
  #entreno en 10 meses,  periodo  [ primero, ultimo ]
  dgeneracion  <-   xgb.DMatrix( data  = data.matrix( dataset[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo , 
                                                              !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]),
                                 label = dataset[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo, clase_ternaria ]
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles
  
  modelo = xgb.train( 
    data= dgeneracion,
    objective= "binary:logistic",
    tree_method= "hist",
    max_bin= 31,
    base_score= mean( getinfo(dgeneracion, "label") ),
    eta= 0.04,
    nrounds= 300, 
    colsample_bytree= 0.6
  )
  
  #aplico a los datos de pred, que tienen la clase vacia
  daplicacion  <-   xgb.DMatrix( data  = data.matrix( dataset[ foto_mes==pred, !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]),
                                 label = dataset[ foto_mes==pred, clase_ternaria ]
  )
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <-  cbind(  dataset[ foto_mes==pred, c("numero_de_cliente","clase_ternaria") ], aplicacion_prediccion )
  
  #le doy nombre a las columnas
  colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )
  
  #para calcular la ganancia, cuando se corre para meses del pasado
  print(paste(pred,":",sum(  prediccion_final[ prob_positivo>0.025,   ifelse( clase01 == 1  , 19500, -500) ] )))
  
  #Genero las TRES salidas
  #grabo todas las probabilidad, simplemente para tenerlo
  setwd(  "~/cloud/cloud1/work/")
  fwrite( prediccion_final[ order( -prob_positivo) ],
          file= paste( "ldm_prob_", pred, ".txt", sep="" ),
          sep="\t",
          eol = "\r\n")

  #grabo la importancia de las variables
  write.table(  xgb.importance( model = modelo ),
                file=paste( "ldm_importancia_", pred, ".txt", sep="" ),
                sep="\t",
                eol = "\r\n"
  )
}
#___________________________________________________________

#cargo los datasets
setwd( "~/cloud/cloud1/datasets/")
dataset   <-   fread( "paquete_premium_hist.txt.gz" )

dataset = fread("paquete_premium_hist_dummy1.txt")

# Lista de todos los foto_mes
vmeses <-  abs(sort(-unique( dataset$foto_mes )))

# Tabla codificando cada foto_mes con un número
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )

# Renombro las columnas de la tabla
colnames( tb_meses ) <- c( "mes", "foto_mes" )

# Agrego la columna "mes" a dataset
dataset[  tb_meses,  on="foto_mes",  mes:= i.mes ]

#dejo la clase en 0,1
dataset[  , clase_ternaria := as.integer(clase_ternaria=="BAJA+2") ]

#agrego variable para el undersampling
set.seed(410551)
dataset[ ,  sample :=  runif( nrow(dataset) )]

# Selección de los meses a procesar
meses_a_procesar  <-   tb_meses[  foto_mes>=201806  & foto_mes<=201904, foto_mes ]

# Aplico la función a la lista de meses
lapply( meses_a_procesar, linea_muerte )
