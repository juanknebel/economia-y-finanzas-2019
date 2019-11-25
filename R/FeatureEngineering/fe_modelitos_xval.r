#En construccion
#AUN NO FUNCIONA


#Este programa genera nuevas variables, que son MODELOS

#No me desespero por que sean excelentes modelos
#Una futura optimizacion bayesiana, en otro script, dira si estas variables son utiles o no.

#Aqui NO SE HACE  optimizacion bayesiana
#Aqui NO hay canaritos
#Aqui NO guardo en hiperparametro los mejores parametros


#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("ROCR")
library("lightgbm")



#raiz del environment
env <- list()

directory <-list()
switch (Sys.info()[['sysname']],
         Windows = { 
                     directory$work     <-  "M:\\work\\"
                     directory$datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { 
                     directory$work     <-  "~/dm/work/"
                     directory$datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { 
                     directory$work     <-  "~/cloud/cloud1/work/"
                     directory$datasets <-  "~/cloud/cloud1/datasets/"
                   }
       )
env$directory <- directory


#Parametros entrada de nuestro dataset
data <- list()
data$campos_separador     <-  "\t"
data$campo_foto           <-  "foto_mes"
data$campo_id             <-  "numero_de_cliente"
data$clase_nomcampo       <-  "clase_ternaria"
data$clase_valor_positivo <-  "BAJA+2"
data$campos_a_borrar      <-  c()
data$archivo_grande       <-  "paquete_premium_exthist.txt.gz"
data$undersampling        <-   0.2
data$ventana              <-  10
env$data <- data


#sobre el funcionamiento de programa
#en el lightgbm
lgb <- list()
lgb$folds                 <-      3
lgb$semilla               <- 102191
lgb$max_bin               <-    255
lgb$subsample             <-      1.0
lgb$num_iterations_max    <-   1000
lgb$early_stopping_round  <-     30
lgb$num_leaves            <-   2048
env$lightgbm <- lgb


problema <- list()
problema$ganancia_acierto     <-  19500 
problema$ganancia_noacierto   <-   -500
env$problema <- problema


#Hacer que la variable   env   NO se pueda modificar
lockBinding( "env", globalenv() )

#------------------------------------------------------------------------------
Gprob_corte <- 0.025

fganancia_logistic_lightgbm   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > Gprob_corte  ) * 
                 ifelse( vlabels== 1, env$problema$ganancia_acierto, env$problema$ganancia_noacierto )   
            )
        

   return(  list(name = "ganancia", value =  ifelse(  is.na(gan) , 0, gan), higher_better= TRUE )  )
}
#------------------------------------------------------------------------------

generar_y_aplicar_modelos  <- function( )
{


  for( mes_cero  in c(1:25) )
  {
  
    #generacion de  dtrain  , que tiene undersampling
    dataset_grande[  , train := 0L ]
    dataset_grande[  (sample < env$data$undersampling | clase01==1) & mes<=((mes_cero+2) + env$data$ventana-1)  &  mes>=(mes_cero+2), 
                     train := 1L ]

    #A este dataset es el que le voy a aplicar TODOS los modelos
    dtrain_under <-   lgb.Dataset( data  = as.matrix(dataset_grande[ train==1, 
                                                                     !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                                     with=FALSE]),
                                  label = dataset_grande[ train==1, clase01 ] ,
                                  free_raw_data=FALSE )


    #generacion de  dtrain_completo  , que NO tiene undersampling
    dataset_grande[  , train := 0L ]
    dataset_grande[   mes<=((mes_cero+2) + env$data$ventana-1)  &  mes>=(mes_cero+2), 
                     train := 1L ]

    #A este dataset es el que le voy a aplicar TODOS los modelos
    dtrain_completo <-  lgb.Dataset( data  = as.matrix(dataset_grande[ train==1, 
                                                                       !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                                       with=FALSE]),
                                     label = dataset_grande[ train==1, clase01 ] ,
                                     free_raw_data=FALSE )

    for( vid  in tb_modelos$id )
    {  
      z <- tb_modelos[ id==vid, ]
      Gprob_corte  <<-   -problema$ganancia_noacierto*(1/env$data$undersampling)/( problema$ganancia_acierto - problema$ganancia_noacierto*(1/env$data$undersampling) )
      

      #aqui hago  CROSS VALIDATION, para determinar el  num_iterations  optimo
      modelo = lgb.cv( 
                       data= dtrain_under,  
                       objective= "binary",
                       nfold= env$lightgbm$folds,
                       stratified= TRUE,
                       eval= fganancia_logistic_lightgbm, 
                       metric= "ganancia" ,
                       num_leaves= env$lightgbm$num_leaves,
                       num_iterations= env$lightgbm$num_iterations_max,
                       early_stopping_rounds= env$lightgbm$early_stopping_round,
                       max_bin= env$lightgbm$max_bin,
                       boost_from_average= TRUE ,
                       subsample= env$lightgbm$subsample, 
                       feature_fraction= z$feature_fraction, 
                       learning_rate= z$learning_rate,
                       min_data_in_leaf= z$min_data_in_leaf, 
                       max_depth= z$max_depth,
                       lambda_l1= z$lambda_l1, 
                       lambda_l2= z$lambda_l2, 
                       min_gain_to_split= z$min_gain_to_split,
                       verbosity= -1,
                       verbose= -1
                     )

      iteraciones <-  modelo$best_iter
      #unlist(modelo$record_evals$valid$ganancia$eval) 
    
    
      #Aqui genero el modelo, con el recien calculado  iteraciones
      mfinal = lgb.train( 
                         data= dtrain_completo,  
                         objective= "binary",
                         num_leaves= env$lightgbm$num_leaves,
                         num_iterations= iteraciones,
                         max_bin= env$lightgbm$max_bin,
                         boost_from_average= TRUE ,
                         subsample= env$lightgbm$subsample, 
                         feature_fraction= z$feature_fraction, 
                         learning_rate= z$learning_rate,
                         min_data_in_leaf= z$min_data_in_leaf, 
                         max_depth= z$max_depth,
                         lambda_l1= z$lambda_l1, 
                         lambda_l2= z$lambda_l2, 
                         min_gain_to_split= z$min_gain_to_split,
                         verbosity= -1,
                         verbose= -1
                        )
 
      #Aplico mfinal a los datos de mes_cero
      prediccion  <- predict( mfinal,  
                              as.matrix( dataset_grande[ mes==mes_cero,
                                                         !c("sample","train","clase01", env$data$campo_id, env$data$clase_nomcampo), 
                                                         with=FALSE ] ) )

      dataset_salida[ mes==mes_cero,  paste0( "modelito_", z$id) :=  prediccion ]

      rm( modelo, mfinal, prediccion )
      gc()
    }
    
    rm( dtrain_under, dtrain_completo)
    gc()
  }
  
  dataset_grande[ , train:= NULL ]
  
}
#------------------------------------------------------------------------------

#cargo los archivos de entrada
setwd( env$directory$datasets)
if( env$data$archivo_grande %like% "gz" ) 
{
  dataset_grande   <- fread(env$data$archivo_grande)
} else {
  dataset_grande   <- fread(cmd=paste("cat", env$data$archivo_grande))
}


#ordeno para que sea facil armar los datasets
setorder(  dataset_grande, foto_mes, numero_de_cliente )

#Borro campos
if( length(env$data$campos_a_borrar)>0 )  dataset_grande[ ,  (env$data$campos_a_borrar) := NULL    ] 

#dejo la clase en {0,1}  
dataset_grande[, clase01 := as.integer( get(env$data$clase_nomcampo) == env$data$clase_valor_positivo  )] 


#agrego variable para el undersampling
set.seed(410551)
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]


#agrego la columna  mes_actual
vmeses   <-  abs(sort(-unique( dataset_grande$foto_mes )))
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )
colnames( tb_meses ) <- c( "mes", "foto_mes" )
dataset_grande[  tb_meses,  on="foto_mes",  mes:= i.mes ]


#Creo el dataset donde almaceno temporariamente las nuevas variables modelo
dataset_salida <-  dataset_grande[ , c("numero_de_cliente", "foto_mes", "mes" ), with=FALSE ]


#Defino z con unos valores iniciales
#Aqui el alumno sagaz condimentara a gusto
z <- list( "feature_fraction"=1.0, "learning_rate"=0.3, "max_depth"=4, "min_data_in_leaf"=10, "min_gain_to_split"=1.0,  "lambda_l1"=10, "lambda_l2"=20)
tb_modelos <-  as.data.table( z )

#Genero parametros muy distintos
for( plearning_rate  in c( 0.3, 0.03 ) )
{
  z$learning_rate <- plearning_rate
  for( pmax_depth  in c( 2, 6, 12 ) )
  {
    z$max_depth <- pmax_depth
    for( pmin_data_in_leaf  in c( 1, 100 ) )
    {
      z$min_data_in_leaf  <- pmin_data_in_leaf
      for( pfeature_fraction  in c( 0.5) )
      {
        z$feature_fraction <- pfeature_fraction
        tb_modelos <-  rbind( tb_modelos,  z )
      }
    }
  }
}

#Asigno un ID a la lista de parametros recien generada
tb_modelos[  , id:= 100:(100+nrow(tb_modelos)-1) ]

#En tb_modelos quedo lo que se va a procesar
tb_modelos

#calculo todos los modelos
#aqui se hace el trabajo pesado
generar_y_aplicar_modelos()


#Grabo el archivo para Stacking
dataset_salida[ , env$data$clase_nomcampo:= dataset_grande[ , get(env$data$clase_nomcampo)]  ]

setwd( env$directory$datasets)
fwrite(  dataset_salida[ , !c("mes"), with=FALSE],
         file="paquete_premium_stackingxval.txt.gz",
         sep="\t"
       )


#Grabo el archivo de salida
setwd( env$directory$datasets)
fwrite(  cbind(  dataset_grande[ , !c("clase01","sample","mes"), with=FALSE],  
                 dataset_salida[ , !c("numero_de_cliente","foto_mes","mes"), with=FALSE ]
              ),
         file="paquete_premium_exthist_modelitosxval.txt.gz",
         sep="\t"
       )
         


#limpio la memoria
rm(list=ls())
gc()


#salgo del R sin grabar el gigante entorno
quit(save="no")

