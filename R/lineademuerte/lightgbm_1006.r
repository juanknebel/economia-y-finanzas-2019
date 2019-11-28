#limpio la memoria
rm(list=ls())
gc()

library( "data.table" )
library( "lightgbm" )

dead_line = function(month_to_process) {
  future = as.numeric(tb_meses[foto_mes == month_to_process, mes])
  end = as.numeric(future+2)
  start = as.numeric(end+9)
  
  initial_month = as.numeric(tb_meses[mes == start, foto_mes])
  end_month = as.numeric(tb_meses[mes == end, foto_mes])
  predict_month = as.numeric(month_to_process)

  degeneracion <- lgb.Dataset( data  = data.matrix(dataset[ foto_mes>=initial_month & foto_mes<=end_month, !c("numero_de_cliente","clase_ternaria") , with=FALSE]),
                             label = dataset[foto_mes>=initial_month & foto_mes<=end_month, clase_ternaria], 
                             free_raw_data=FALSE )

  set.seed( 209809 )
  
  # Experimento 17103
  # ventana=10, probcorte=0.201105747846206, num_iterations=6, learning_rate=0.228688688639827,
  # lambda_l1=24.3594926740932, lambda_l2=26.705772921457, min_gain_to_split=4.87267173742336,
  # min_data_in_leaf=95, max_depth=8, feature_fraction=0.995789493300305, max_bin=31, subsample=1



  the_model = lgb.train( 
    data = degeneracion,
    objective = "binary",
    metric="auc",
    seed= 209809,
    num_iterations=6, 
    boost_from_average=FALSE,
    bagging_fraction=1, 
    feature_fraction=0.9957, 
    learning_rate=0.2286, 
    min_child_weight=8, 
    max_depth=8, 
    lambda_l1=24.3594,
    lambda_l2=26.7057,
    max_bin=31, 
    num_leaves=255)

  #aplico el modelo a datos nuevos
  daplicacion  <-  dataset[ foto_mes==predict_month, !c("numero_de_cliente","clase_ternaria")]
  aplicacion_prediccion  <- predict(  the_model, as.matrix(daplicacion ))

  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <-  cbind(  dataset[ foto_mes==predict_month, c("numero_de_cliente","clase_ternaria") ], aplicacion_prediccion )

  #le doy nombre a las columnas
  colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )

  #Genero las TRES salidas default para todos los meses
  #grabo todas las probabilidad, simplemente para tenerlo
  setwd(  "~/cloud/cloud1/work/")
  probabilities = prediccion_final[ order( -prob_positivo) ]
  probabilities[, (c("foto_mes")) := predict_month]
  fwrite(probabilities, 
         file=paste0(file_name, "_", month_to_process, "_probabilidades.txt"), 
         sep="\t", 
         eol = "\r\n")

  #Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
  #me quedo solamente con los numero_de_cliente donde probabilidad > prob_class_down
  fwrite(as.data.table( prediccion_final[ prob_positivo > prob_class_down  , "numero_de_cliente" ] ), 
         file=paste0(file_name, "_", month_to_process, "_entregar.txt"), 
         col.names=FALSE, 
         sep="\t", 
         eol = "\r\n")

  #grabo la importancia de las variables
  write.table(lgb.importance( model = the_model ),
              file=paste0(file_name, "_", month_to_process, "_importancia.txt"),
              sep="\t",
              eol = "\r\n")

  # Solo si el mes no es el de la competencia devuelve la ganancia
  if (predict_month != competition_month) {
    the_profit = sum(prediccion_final[ prob_positivo>prob_class_down,   ifelse( clase01 == 1  , 19500, -500) ])
    print(paste0("Profit for month ", predict_month, " is ", the_profit))
    actual_profit = data.table( month = c(predict_month), profit = c(the_profit) )
    fwrite(actual_profit,
           file=paste0(file_name,"_ganancias_actuales.txt"),
           sep="\t",
           eol = "\r\n",
           append = TRUE)
    # Si la ganancia de este mes actual no fue mejor que la calculada por el teacher paro la ejecucion
    threshold_profit = profits_to_compare[month == predict_month, "profit"]
    if (the_profit < threshold_profit) {
      message = paste0("La ganancia no supera el umbral", "\nGanancia actual: ", the_profit, "\nGanancia del umbral: ", threshold_profit)
      # stop(message)
    }
  }
}

#------------------------------------
setwd( "~/cloud/cloud1/datasets/")
dataset <- fread( "paquete_premium_exthist_lm_original.txt.gz" )
file_name = "lightgbm_dead_line_1004"
from = 201706
to = 201904
competition_month = 201906
# La probabilidad default de corte
prob_class_down = 0.025
# prob_class_down = 0.0265

profits_to_compare = data.table(
  month = c(201806, 201807, 201808, 201809, 201810, 201811, 201812, 201901, 201902, 201903, 201904),
  profit = c(12505500, 10298000, 11132500, 11939500, 9620500, 10382500, 11073000, 9479000, 10528000, 10423000, 9122500),
  positives = c(1294, 1201, 1222, 1197, 1010, 1070, 1103, 985, 1085, 1119, 918)
  )

vmeses <-  abs(sort(-unique( dataset$foto_mes )))

# Tabla codificando cada foto_mes con un número
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )

# Renombro las columnas de la tabla
colnames( tb_meses ) <- c( "mes", "foto_mes" )

# Agrego la columna "mes" a dataset
dataset[  tb_meses,  on="foto_mes",  mes:= i.mes ]

#dejo la clase en 0,1
dataset[  , clase_ternaria := as.integer(clase_ternaria=="BAJA+2") ]

# Selección de los meses a procesar
meses_a_procesar <- tb_meses[  foto_mes>=from  & foto_mes<=to, foto_mes ]

dead_line(competition_month)

# Aplico la función a la lista de meses
lapply( meses_a_procesar, dead_line )
