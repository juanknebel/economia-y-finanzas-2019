#El objetivo es mejorar la ganancia sobre 201904 (ultimo mes con clase)
#usando codigo R muy simple y no parametrizado
#siempre aplico el modelo a datos del futuro, que NO fueron vistos para entrenar


#Cargo las librerias que voy a utilizar
library( "data.table" )
library( "rpart" )


#cargo el dataset
#se hace el cat  para que se lea rapido el archivo del bucket
setwd( "M:\\work\\")
dataset_grande <-  fread( "paquete_premium_dias_1ano.txt" )


#ordeno el dataset, para poder crear variables del tipo LAG
setorder(  dataset_grande, numero_de_cliente, foto_mes )

#------------------------------------------------------------
#determino el dataset donde voy a aplicar el modelo
dataset_abril <- dataset_grande[ foto_mes ==201904, ]


#agrego un campo que es la ganancia, para facilitar los calculos
dataset_abril[ clase_ternaria=="BAJA+2", ganancia:= 19500 ]
dataset_abril[ clase_ternaria!="BAJA+2", ganancia:=  -500 ]


#------------------------------------------------------------
#Capitulo 1  Entrenar en un solo mes

#ahora genero el dataset donde voy a generar el modelo, ENTRENAR
#por ahora un solo mes

dataset_generacion <- dataset_grande[ foto_mes==201902, ]
nrow( dataset_generacion )


#genero el modelo, construyo en modelo
modelo  <- rpart( clase_ternaria ~ .   ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 12,
                  minsplit= 20,
                  minbucket= 5
                  )



#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  (prob_positivo > 0.025)

#calculo la ganancia que me va a dar cada registro
#queda CERO en un registro si no tengo que enviar, porque prob_positivo<0.025
#queda 19500  si le envio a un BAJA+2
#queda  -500  si le envio a un CONTINUA
#queda  -500  si le envio a un BAJA+1

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia1 <-  sum( vector_ganancias )
ganancia1

#Apenas una ganancia de 6357000  , LAMENTABLE 

#------------------------------------------------------------
#Capitulo 2  Entrenar en SEIS meses del pasado

dataset_generacion <- dataset_grande[ foto_mes>=201809 & foto_mes<=201902, ]
nrow( dataset_generacion )


#genero el modelo, construyo en modelo
modelo  <- rpart( clase_ternaria ~ .   ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 12,
                  minsplit= 20,
                  minbucket= 5
                  )



#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia2 <-  sum( vector_ganancias )
ganancia2

#Pasamos de los lamentables 6357000  a muy respetables 6830000 

#ahora veo cuales son las variables mas importantes del arbol
setwd( "M:\\work\\")
summary( modelo , file="modelo2.txt" )

#desde el bucket,  leer el archivo modelo2.txt
#Que variable sale como  la primera  mas importante el arbol ?
#Y la segunda ?

#------------------------------------------------------------
#Capitulo 3   unir  Visa_cuenta_estado y Master_cuenta_estado

dataset_generacion[  , tarjeta_cuenta_estado:= pmax( Visa_cuenta_estado, Master_cuenta_estado, na.rm=TRUE ) ]
dataset_abril[     , tarjeta_cuenta_estado:= pmax( Visa_cuenta_estado, Master_cuenta_estado, na.rm=TRUE ) ]

#genero el modelo, construyo en modelo
modelo <-  rpart( clase_ternaria ~ .   ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 12,
                  minsplit= 20,
                  minbucket= 5
                  )

#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia3 <-  sum( vector_ganancias )
ganancia3

#Pasamos de 6830000  a  7055500  (un humilde incremento de 225500 )

#Que tan importante es en el nuevo modelo la recien creada tarjeta_cuenta_estado ?
setwd( "M:\\work\\")
summary( modelo , file="modelo3.txt" )

#viendo el archivo  modelo3.txt , vemos que tarjeta_cuenta_estado es la variable MAS IMPORTANTE
#como segunda variable mas importante aparece  mcuenta_corriente_Paquete


#------------------------------------------------------------
#Capitulo 4  Variable derivada de  mcuenta_corriente_Paquete

#calculo el valor del mes anterior
dataset_generacion[  ,  mcuenta_corriente_Paquete_1 := shift(mcuenta_corriente_Paquete, 1),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuenta_corriente_Paquete_1 := shift(mcuenta_corriente_Paquete, 1),  by=c("numero_de_cliente") ]  

dataset_generacion[  ,  mcuentas_saldo_1 := shift(mcuentas_saldo, 1),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuentas_saldo_1 := shift(mcuentas_saldo, 1),  by=c("numero_de_cliente") ]  


#A partir de aqui que el alumno continue
  
#genero el modelo, construyo en modelo
modelo <-  rpart( clase_ternaria ~ .   ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 12,
                  minsplit= 20,
                  minbucket= 5
                  )

#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia4 <-  sum( vector_ganancias )
ganancia4

#Pasamos de 7055500 a 7023000  ( Descendimos !  )

#Que tan importante es en el nuevo modelo  ?
setwd( "M:\\work\\")
summary( modelo , file="modelo4.txt" )


# mcuenta_corriente_Paquete_1 aparece como TERCER variable en importancia
# mcuentas_saldo_1  aparece en QUINTO lugar de importancia

#------------------------------------------------------------
#Capitulo 5  Tendencia ultimos 5 meses de  mcuenta_corriente_Paquete  y  mcuentas_saldo

#se pide a los alumnos que utilizando la funcion shift calculen los 5 meses previos
#y basados en la formula de  https://www.mathsisfun.com/data/least-squares-regression.html
#calcular la tendencia


dataset_generacion[  ,  mcuentas_saldo_2 := shift(mcuentas_saldo, 2),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuentas_saldo_2 := shift(mcuentas_saldo, 2),  by=c("numero_de_cliente") ]  
dataset_generacion[  ,  mcuentas_saldo_3 := shift(mcuentas_saldo, 3),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuentas_saldo_3 := shift(mcuentas_saldo, 3),  by=c("numero_de_cliente") ]  
dataset_generacion[  ,  mcuentas_saldo_4 := shift(mcuentas_saldo, 4),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuentas_saldo_4 := shift(mcuentas_saldo, 4),  by=c("numero_de_cliente") ]  
dataset_generacion[  ,  mcuentas_saldo_5 := shift(mcuentas_saldo, 5),  by=c("numero_de_cliente") ]  
dataset_abril[     ,  mcuentas_saldo_5 := shift(mcuentas_saldo, 5),  by=c("numero_de_cliente") ]  

( Se deja este capitulo enteramente a los alumnos )

#------------------------------------------------------------
#Capitulo 6  Generar nuevas variables que sean modelos de meses anteriores  
# 


#Septiembre  -----------------
modelo_sep <-  rpart( clase_ternaria ~ .   ,
                      data = dataset_grande[foto_mes==201809, ] , 
                      xval= 0,
                      cp= 0,
                      maxdepth= 12,
                      minsplit= 20,
                      minbucket= 5
                    )

#aplico el modelo de septiembre  al dataset de generacion
sep_app  <- predict( modelo_sep, dataset_generacion , type = "prob")
sep_prob <- sep_app[ , "BAJA+2"]
#creo la columna en  dataset_generacion
dataset_generacion[  , arbol_sep:= sep_prob]

#aplico el modelo de septiembre al dataset de abril
sep_app  <- predict( modelo_sep, dataset_abril , type = "prob")
sep_prob <- sep_app[ , "BAJA+2"]
#creo la columna en  dataset_abril
dataset_abril[  , arbol_sep:= sep_prob]


#Dejo a los alumnos continuar de Octubre  a Febrero inclusive



#genero el modelo, construyo en modelo
modelo <-  rpart( clase_ternaria ~ . ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 10,
                  minsplit= 20,
                  minbucket= 5
                  )

#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia6 <-  sum( vector_ganancias )
ganancia6

#Pasamos de 7055500 a 7023000  ( nada interesante !  )

#Pero aqui SI aparece lo interesante !
#Que variables son las mas importantes  ?
setwd( "M:\\work\\")
summary( modelo , file="modelo6.txt" )


#------------------------------------------------------------
#Capitulo 7  Utilizando solo las nuevas variables ?  
# 


#genero el modelo, construyo en modelo
#SOLO a partir de los nuevos campos, sin considerar el resto
modelo <-  rpart( clase_ternaria ~ arbol_sep +arbol_oct +arbol_nov +arbol_dic +arbol_ene +arbol_feb ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 10,
                  minsplit= 20,
                  minbucket= 5
                  )

#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia7 <-  sum( vector_ganancias )
ganancia7

# pasamos de 7023000 a 6831500 ,  retrocedimos !


#Que variables son las mas importantes  ?
setwd( "M:\\work\\")
summary( modelo , file="modelo7.txt" )



#------------------------------------------------------------
#Capitulo 8  Y si agregamos una variable que sea el promedio de 6 modelos de arboles  
# 


dataset_generacion[ , arbol_promedio:= (arbol_sep +arbol_oct +arbol_nov +arbol_dic +arbol_ene +arbol_feb)/6 ]
dataset_abril[    , arbol_promedio:= (arbol_sep +arbol_oct +arbol_nov +arbol_dic +arbol_ene +arbol_feb)/6 ]


#Ahora genero un nuevo modelo, con el dataset con las nuevas variables

#genero el modelo, construyo en modelo
modelo <-  rpart( clase_ternaria ~ arbol_promedio + arbol_sep +arbol_oct +arbol_nov +arbol_dic +arbol_ene +arbol_feb    ,
                  data = dataset_generacion, 
                  xval= 0,
                  cp= 0.0,
                  maxdepth= 10,
                  minsplit= 20,
                  minbucket= 5
                  )

#aplico el modelo a datos nuevos, fijarse que queda en  aplicacion_predicion
aplicacion_prediccion  <- predict( modelo, dataset_abril , type = "prob")

#me quedo con la columna de la probabilidad de "BAJA+2",  fijarse que queda en prob_positivo
prob_positivo <-  aplicacion_prediccion[ , "BAJA+2"]

#ahora genero un vector que dice si hay que enviar estimulo o no
enviar_estimulo  <-  prob_positivo > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia8 <-  sum( vector_ganancias )
ganancia8


#Ganancia 7140500  

#Que variables son las mas importantes  ?
setwd( "M:\\work\\")
summary( modelo , file="modelo8.txt" )



#------------------------------------------------------------
#Capitulo 9  Trabajar directamente con el arbol  promedio    
# 

enviar_estimulo  <-  dataset_abril$arbol_promedio > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia9 <-  sum( vector_ganancias )
ganancia9

#Ganancia  7386000  superior incluso a la ultima de 7140500  


#------------------------------------------------------------
#Capitulo 10   y si trabajo con un promedio ponderado 
# 
#esta tarea es en honor a Denise Silvana

#primero, una ponderacion  grosera

dataset_generacion[ , arbol_denise:= (0.5*arbol_sep +0.6*arbol_oct +0.7*arbol_nov +0.8*arbol_dic +0.9*arbol_ene +1.0*arbol_feb)/(0.5+0.6+0.7+0.8+0.9+1.0) ]
dataset_abril[    , arbol_denise:= (0.5*arbol_sep +0.6*arbol_oct +0.7*arbol_nov +0.8*arbol_dic +0.9*arbol_ene +1.0*arbol_feb)/(0.5+0.6+0.7+0.8+0.9+1.0) ]


enviar_estimulo  <-  dataset_abril$arbol_denise > 0.025

vector_ganancias <-  enviar_estimulo * dataset_abril$ganancia


#finalmente calculo la ganancia
ganancia10 <-  sum( vector_ganancias )
ganancia10

#Ganancia  7368500 , apenas inferior a  7386000
#Denise, como se podria cambiar la ponderacion para aumentar la ganancia ?  


#------------------------------------------------------------
#Capitulo 11   voto de la mayoria 
# 
#En el Capitulo 9  se trabajo con el arbol promedio
#Que tal si partiendo de los arboles  arbol_mar, arbol_abr, ... arbol_ago
#Se elige un registro si este es elegido en octubre por mas de 3 arboles ?
#Un arbol elige un registro, si a ese registro le asigna una prob mayor a 0.025

#Se deja que los alumnos encaren SIN ASISTENCIA  esta tarea


