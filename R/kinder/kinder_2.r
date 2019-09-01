#en la salita de 2 años NO se usan variables, tampoco sentencia if, loops ni funciones, ni parametros


#R para Kindergarten, sala de 2 años (pero con conceptos no triviales de analisis de datos)
#entorno de ejecucion : PC local

#set working directory
setwd( "M:\\datasets\\" )


#opcion nativa en R para cargar un dataset

#cargo los datos, forma estandar
t0       <-  Sys.time()
dataset  <-  read.table("201902.txt", sep="\t")
t1       <-  Sys.time()

#calculo el tiempo de corrida, expresado en segundos
tcorrida <-  as.numeric(t1 - t0, units = "secs")
cat("tiempo carga dataset con read.table ",  tcorrida, "\n" )


#opcion con  libreria  data.table

#instalo el paquete, se hace una sola vez
install.packages( "data.table" )
#invoco a la libreria
library( "data.table" )

#cargo los datos, con la funcion File Read de la libreria data.table
t0       <-  Sys.time()
dataset  <-  data.table::fread("201902.txt")
t1       <-  Sys.time()
tcorrida <-  as.numeric(t1 - t0, units = "secs")
cat("tiempo carga dataset con fread ",  tcorrida, "\n" )


#se observa una enorme diferencia en el tiempo de carga de un archivo
# A partir de ahora usamos la libreria  data.table para manejar los datasets

#Notar como se llamó a la funcion data.table::fread() especificando la libreria
#tambien se la podria haber llamado  directamente con fread() 


#------------------------------------------------------------------------------
#operaciones basicas a un dataset usando  data.table


#cantidad de columnas
ncol( dataset )

#nombres de las columnas
colnames( dataset )

#cantidad de registros
nrow( dataset )

#ver que hay dentro
dataset

#ver un resumen
summary( dataset )

#si quiero que el summary quede en un archivo
setwd( "M:\\work\\" )
sink("summary.txt")
summary( dataset )
sink()

#calculo del promedio
mean(  dataset[  , mcuentas_saldo ] )
#otra forma 
mean(  dataset$mcuentas_saldo )


#calculo de la mediana
median(  dataset[  , mcuentas_saldo ] )

#minimo y maximo de una columna
#tener en cuenta que esto incluye los saldos en pesos y dolares (pero expresados en pesos argentinos)
min(  dataset[  , mcuentas_saldo ] )
max(  dataset[  , mcuentas_saldo ] )


#ver un campo en particular Frequency Table
ftable( dataset$clase_ternaria )
ftable( dataset$Visa_cuenta_estado )
ftable( dataset$Master_cuenta_estado )

#Tabla de contingencia de un campo versus la clase
dcast( dataset, Visa_cuenta_estado  ~ clase_ternaria, length )
dcast( dataset, Master_cuenta_estado  ~ clase_ternaria, length )

#Tabla de contingencia de dos campos versus la clase
dcast( dataset, Visa_cuenta_estado + Master_cuenta_estado ~ clase_ternaria, length )

#-----------------------------------
#Verificar como los valores siguientes
#surgen directamente o son sumas
#de las celdas de la tabla de contingencia  dcast( dataset, Visa_cuenta_estado  ~ clase_ternaria, length )

#filtrar registros con condiciones logicas
#notar como se utiliza el == 
#notar que el AND  es  &
#notal que el OR   es  |

nrow(  dataset[  Visa_cuenta_estado==19, ] )
#la negacion
nrow(  dataset[  !(Visa_cuenta_estado==19), ] )

nrow(  dataset[  Visa_cuenta_estado==19 & clase_ternaria=="BAJA+2", ] )
nrow(  dataset[  Visa_cuenta_estado==10 & clase_ternaria=="BAJA+2", ] )
nrow(  dataset[  !(Visa_cuenta_estado==10) & clase_ternaria=="BAJA+2", ] )

#notar como se deben poner los parentesis porque el operador "&" tiene mas precedencia que el operador  "|"
nrow(  dataset[  (Visa_cuenta_estado==11 | Visa_cuenta_estado==12 ) & clase_ternaria=="BAJA+2", ] )
#en forma equivalente se puede hacer  
nrow(  dataset[  (Visa_cuenta_estado %in% c( 11, 12))  & clase_ternaria=="BAJA+2", ] )

#la condicion para ver si un campo es nulo
nrow(  dataset[  is.na(Visa_cuenta_estado) & clase_ternaria=="BAJA+2", ] )

#la condicion para que un campo NO sea nulo
nrow(  dataset[  !is.na(Visa_cuenta_estado) & clase_ternaria=="BAJA+2", ] )


#-----------------------------------
#Filtrando columnas
#si ademas quiero filtrar columnas
dataset[  Visa_cuenta_estado==19 & clase_ternaria=="BAJA+2", c( "Visa_cuenta_estado", "Master_cuenta_estado", "mcuentas_saldo", "clase_ternaria" )] 

#-----------------------------------
#crear un nuevo atributo en una data.table

#primero unos atributos basicos
#Notar como NO se usa la sentencia if
dataset[  , clase01 := 0 ]
dataset[ clase_ternaria=="BAJA+2",  clase01 := 1 ]

dataset[  , ganancia := -500]
dataset[ clase_ternaria=="BAJA+2",  ganancia := 19500 ]

#si le enviara estimulo a toda la base
sum(  dataset$ganancia )

#-----------------------------------
#aplico lo anterior a un predicado interesante
# Predicado1 =  Visa_cuenta_estado vale 11, 12 o 19  o  es NULO
#nada mal la ganancia de este simple predicado
sum(  dataset[ is.na( Visa_cuenta_estado ) |  (Visa_cuenta_estado %in% c(11,12,19) ),   ganancia ] )

#para que va a contratar la empresa a un Data Scientist si con este simpre predicado gana millones ?


#La cantidad de registros de ese predicado
nrow( dataset[ is.na( Visa_cuenta_estado ) |  (Visa_cuenta_estado %in% c(11,12,19) ), ] )

#La cantidad de positivos de ese predicado
sum( dataset[ is.na( Visa_cuenta_estado ) |  (Visa_cuenta_estado %in% c(11,12,19) ), clase01 ] )

#O lo puedo ver directamente con  dcast
#primero creo un nuevo atributo
dataset[  , predicado1 := 0 ]
dataset[ is.na( Visa_cuenta_estado ) |  (Visa_cuenta_estado %in% c(11,12,19) ) , predicado1 := 1 ]
dcast( dataset, predicado1 ~ clase_ternaria, length )

#-----------------------------------
#Ahora otre predicade buene y hermose

dataset[  , predicado2 := 0 ]
dataset[ tmovimientos_ultimos90dias <14  , predicado2 := 1 ]
sum( dataset[ predicado2==1, ganancia ] )

#Podria haber hecho
dataset[  , predicado3 := FALSE ]
dataset[ tmovimientos_ultimos90dias <14  , predicado3 := TRUE ]
sum( dataset[ predicado3==TRUE, ganancia ] )
sum( dataset[ (predicado3), ganancia ] )

#Ganancia muy buena para un predicado encontrado en la inspiracion ...


#------------------------------------------------------------------------------
#A pesar de estar en salita de 2 años, pasamos a conceptos mineros un poco mas avanzados
#jugando con la variable  tmovimientos_ultimos90dias

#cuantos valores distintos tiene tmovimientos_ultimos90dias
nrow( unique(  dataset[ , c("tmovimientos_ultimos90dias")] ) ) 
#o tambien podria hacer
length( unique( dataset$tmovimientos_ultimos90dias) )

#agrupo  en data.table 
dataset[, sum(ganancia), by = tmovimientos_ultimos90dias]

#ahora que me nombre la nueva variable
dataset[, list( gan=sum(ganancia)), by = tmovimientos_ultimos90dias]

#ahora qen una tabla
tbl <- dataset[, list( gan=sum(ganancia)), by = tmovimientos_ultimos90dias]
setorder( tbl, tmovimientos_ultimos90dias )

#ahora acumulo
tbl[  , gan_acum := cumsum( gan ) ]

tbl[ 1:30, ]

#grafico

#hago el primer grafico
plot( tbl$tmovimientos_ultimos90dias, 
      tbl$gan_acum,       
      type="n",
      main="Ganancia ordenando por tmovimientos_ultimos90dias",
      xlab="tmovimientos_ultimos90dias", 
      ylab="Ganancia Acumulada", 
      pch=19)

lines( tbl$tmovimientos_ultimos90dias, tbl$gan_acum,   type="l" , col="blue", lwd=2)



#me fui de escala, ahora grafico los primeros
plot( tbl$tmovimientos_ultimos90dias[1:30], 
      tbl$gan_acum[1:30],       
      type="n",
      main="Ganancia ordenando por tmovimientos_ultimos90dias",
      xlab="tmovimientos_ultimos90dias", 
      ylab="Ganancia Acumulada", 
      pch=19)

lines( tbl$tmovimientos_ultimos90dias[1:30], tbl$gan_acum[1:30],   type="l" , col="blue", lwd=2)




#------------------------------------------------------------------------------
#ahora analizo la variable  mcuentas_saldo

#cuantos valores distintos tiene tmovimientos_ultimos90dias
nrow( dataset )
nrow( unique(  dataset[ , c("mcuentas_saldo")] ) ) 
nrow( dataset[ mcuentas_saldo==0, ] )

#este animal es de otra especie !


#primero veo si hay nulos
nrow(  dataset[ is.na( mcuentas_saldo ), ] )
#no los hay en este caso ! asi evito angustias de alumnos 


min( dataset$mcuentas_saldo )
max( dataset$mcuentas_saldo )


#ordeno el dataset por la variable  mcuentas_saldo
#setorder es de la libreria  data.table 
setorder(  dataset, mcuentas_saldo )

dataset[  ,      idx := seq( nrow(dataset) ) ]
dataset[  , pos_acum := cumsum( clase01 ) ]
dataset[  , neg_acum := cumsum( (1-clase01) ) ]
dataset[  , gan_acum := cumsum( ganancia ) ]

#veo como quedaron los campos
dataset[ 1:30,  c("mcuentas_saldo", "clase_ternaria", "idx", "clase01", "ganancia", "pos_acum", "neg_acum", "gan_acum" ) ]
#el minero con ojo avezado se da cuenta que hay muchos BAJA+1 y BAJA+2 al comienzo


#hago el primer grafico
plot( dataset$idx, 
      dataset$gan_acum,       
      type="n",
      main="Ganancia ordenando por mcuentas_saldo",
      xlab="registros", 
      ylab="Ganancia Acumulada", 
      pch=19)

lines( dataset$idx, dataset$gan_acum,   type="l" , col="blue", lwd=2)


#se fue de escala para ver el maximo,  me fijo solo en los 15000  primeros registros
plot( dataset$idx[1:15000], 
      dataset$gan_acum[1:15000],
      type="n",
      main="Ganancia ordenando por mcuentas_saldo",
      xlab="registros ordenados por mcuentas_saldo", 
      ylab="Ganancia Acumulada", 
      pch=19)

lines( dataset$idx[1:15000], dataset$gan_acum[1:15000],   type="l" , col="blue", lwd=2)

#busco realmente donde esta el maximo
which.max(  dataset$gan_acum )

dataset[ which.max(  dataset$gan_acum ),  c("mcuentas_saldo", "clase_ternaria", "idx", "clase01", "ganancia", "pos_acum", "neg_acum", "gan_acum" ) ]

#-----------------------------------
#tecnicamente hablando, no es del todo correcto ordenar por un campo
#ya que para por ejemplo a dos registros con el mismo valor de mcuentas_saldo
#setoder los dejo en el orden inicial

#mi deseo es ordenar al azar cuando dos registros tienen el mismo valor de la variable

#genero un vector al valores al azar con distribucion uniforme en el intervalo real continuo [0,1]
runif( 10 )

#ahora agrego una variable al azar al dataset
dataset[  , azar1 :=  runif( nrow(dataset) ) ]

#ordeno primero por mcuentas_saldo  y cuando valen lo mismo por azar1
setorder(  dataset, mcuentas_saldo, azar1 )

which.max(  dataset$gan_acum )
dataset[ which.max(  dataset$gan_acum ),  c("mcuentas_saldo", "clase_ternaria", "idx", "clase01", "ganancia", "pos_acum", "neg_acum", "gan_acum" ) ]

graphics.off()

plot( dataset$idx[1:15000], 
      dataset$gan_acum[1:15000],       
      type="n",
      main="Ganancia ordenando por mcuentas_saldo",
      xlab="registros ordenados por mcuentas_saldo", 
      ylab="Ganancia Acumulada", 
      pch=19)

lines( dataset$idx[1:15000], dataset$gan_acum[1:15000],   type="l" , col="blue", lwd=2)


#------------------------------------------------------------------------------
#ahora creamos variables nuevas

dataset[ , MV_cuenta_estado :=  pmax(Visa_cuenta_estado, Master_cuenta_estado, na.rm = TRUE) ]

#Notar que el maximo de una columna es max , sin embargo el maximo de dos valores es pmax
# na.rm  es  Remove NA's ,  si uno es nulo, se queda con el otro valor

dataset[  , Visa_mconsumototal_rel :=  Visa_mconsumototal  / Visa_mlimitecompra ]

#deduzca cual es la diferencia entre las siguientes dos instrucciones
dataset[  , MV_mconsumospesos  :=   Master_mconsumospesos +  Visa_mconsumospesos ]
dataset[  , MV_mconsumospesos2 :=   rowSums( cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm=TRUE) ]


#------------------------------------------------------------------------------
#ahora pasamos a dividir un dataset en training y testing, 70% y 30%
#como estamos en salita de 2 años, no parametrizamos

#una primera aproximacion
dataset[  ,  azar1 := runif( nrow(dataset) ) ]

dataset[ , fold := 0 ]
dataset[ azar1 > 0.7,  fold := 1 ]

tbl <- dcast( dataset, fold ~ clase_ternaria, length )
tbl[  , pos_idx :=  get("BAJA+2")/( get("BAJA+1")+get("BAJA+2")+get("CONTINUA")) ]
tbl

#------------------------------------------------------------------------------
#eliminar columnas de un dataset
colnames( dataset )

dataset[ , MV_mconsumospesos:= NULL ]
colnames( dataset )

dataset[ , c("Visa_mconsumototal_rel","MV_mconsumospesos2"):= NULL ]
colnames( dataset )

#------------------------------------------------------------------------------
#Borrar toda la memoria para empezar de cero

#Listo los elementos que hay ocupando memoria
ls()

#borrar los elementos que hay en memoria con Remove
rm( list=ls() )

#veo que objetos quedaron ahora en la memoria
ls()

#pero estos objetos siguen ocupando memoria, tengo que llamar al Garbage Collection
gc()

#el comando completo que borra y libera memoria
rm( list=ls() )
gc()


#------------------------------------------------------------------------------
#juego con DOS datasets, lo mas avanzado que veo al final de la salida de 2 años

#borro todo y libero
rm( list=ls() )
gc()

#cargo los dos datasets
setwd( "M:\\datasets\\" )
dataset1 <- fread( "201902.txt" )
dataset2 <- fread( "201904.txt" )

#El objetivo es para 201902  armar la clase que tenga 
# BAJA+1   bajas durante 201903
# BAJA+2   bajas durante 201904
# BAJA+3   bajas durante 201905
# CONTINUA que al  31-05-2019 a las 23:59:59 la persona seguia teniendo Paquete Premium

#Left JOIN  usando  data.table
#A la tabla dataset1  le pego informacion de  dataset2, haciendo el join por el campo numero_de_cliente
dataset1[ dataset2,  on="numero_de_cliente",   clase_201904:= i.clase_ternaria ]

#asi quedo
dataset1[   ,  (cantidad= .N)  ,by=c("clase_ternaria", "clase_201904" ) ]

#opsss,  hay una inconsistencia, un muerto en marzo volvio a vivir en abril !
#pasara esto para otros meses
#Bienvenidos a la imperfeccion de los datos !

#finalmente calculo el campo que queiro
dataset1[  ,  clase_cuaternaria :=  clase_ternaria ]
dataset1[ clase_ternaria=="CONTINUA" &  clase_201904=="BAJA+1" ,  clase_cuaternaria :=  "BAJA+3" ]

#finalmente, veo que todo este bien
dataset1[   ,  (cantidad= .N)  ,by=c("clase_ternaria", "clase_201904", "clase_cuaternaria" ) ]


#------------------------------------------------------------------------------
#Enhorabuena  !
#Felices Vacaciones

#Si ha entendido todo hasta aqui, esta en condiciones de avanzar a la salita de 3 años del kindergarten



