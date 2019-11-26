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

  # Experimento 4511, pero usando la prob de corte del experimento 1004
  # ventana=12, num_iterations=146, learning_rate=0.0386230382853241, 
  # lambda_l1=12.3579491410776, lambda_l2=12.6011508304529, min_gain_to_split=7.23611023655249, 
  # min_data_in_leaf=34, max_depth=13, feature_fraction=0.986007285754121, max_bin=255, subsample=1


  the_model = lgb.train( 
    data = degeneracion,
    objective = "binary",
    metric="auc",
    seed= 209809,
    num_iterations=146, 
    boost_from_average=FALSE,
    bagging_fraction=1, 
    feature_fraction=0.9860, 
    learning_rate=0.0386, 
    min_child_weight=8, 
    max_depth=13, 
    lambda_l1=12.3579,
    lambda_l2=12.6011,
    max_bin=255, 
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
  fwrite(prediccion_final[ order( -prob_positivo) ], 
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
dataset <- fread( "paquete_premium_exthist.txt.gz" )
#dataset <- fread( "~/git/economia-y-finanzas-2019/datasets/paquete_reducido2.csv" )
file_name = "lightgbm_dead_line_1005"
from = 201806
to = 201904
competition_month = 201906
# La probabilidad default de corte
# prob_class_down = 0.025
prob_class_down = 0.0265

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
