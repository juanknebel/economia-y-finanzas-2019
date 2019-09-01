#R para Kindergarten, sala de 3 años 
#ejecutar linea a linea y ver los resultados
#lo minimo de vectores, listas y estructuras de control, para sobrevivir a la materia


#entorno de ejecucion : PC local

#set working directory
setwd( "M:\\datasets\\" )


#opcion nativa en R para cargar un dataset

dataset  <-  fread("201902.txt")

#------------------------------------------------------------------------------
#Vectores


v1  <-  1:10
v1


v2  <-  seq(10)
v2

v3  <-  rep( 3, 5 )
v3

#distribucion uniforme en intervalo (0,1)
v4  <-  runif( 5 )
v4


Notar que v1 y v2 son lo mismo


#agrego un elemento a un vector
v1
v1 <- c( v1, 20 )
v1



#accedo por el indice del vector
v2 <- 21:30
v2

v2[ 1 ] 
v2[ 3 ] 
v2[ 4 ] 

#----------------
#sumar un escalar

v1 <-  1:10
v1 <- v1 + 5
v1

#multiplicar por un escalar
v1 <-  1:10
v1 <- v1*2
v1



#----------------
#concateno dos vectores

v1 <- 1:10
v2 <- 21:30



v3 <- c( v1, v2 )
v3

#largo de un vector
length( v3 )

#esto NO funciona
nrow( v3 )




#------------------------------------------------------------------------------
#Listas

alumno  <-  list( edad=33, sexo="F", grado="Economia", sabe_R=TRUE )

alumno
summary( alumno )

alumno$edad

#agrego un campo a la lista
alumno$sabe_Python=FALSE

alumno


alumno[1]
alumno[2]
alumno[3]

is.list( alumno )



#Otra forma de ver la lista
alumno2  <-  list( 33, "F", "Economia", TRUE, FALSE )
alumno2

names( alumno2 ) <- c( "edad", "sexo", "grado", "sabe_R", "sabe_Python" )

alumno2

#------------------------------------------------------------------------------
#Lista a vector

lprimos <- list(  2,3, 5, 7, 11, 13, 17, 19, 23 )

vprimos <- unlist( lprimos )


lprimos
vprimos
#------------------------------------------------------------------------------
#LOOPS
#por favor, prestar atencion quienes no han programado en su vida


#Aqui vienen distintos loops, que imprimen de diversas formas el resultado


for( i  in   c( "a", "b", "c", "d" )  )
{
  print( i )
}


for( i  in   c( "a", "b", "c", "d" )  )
{
  cat( i )
}



for( i  in   c( "a", "b", "c", "d" )  )
{
  cat( i, " " )
}


for( i  in   1:10  )
{
  cat( i, "\n" )
}


#----------------
#ahora, los temibles Loops Anidados

for( i  in   c( "a", "b", "c", "d" )  )
{
  for( j in 1:5 )
  {
    cat( i,j,  "\n" )
  }
}


#otra forma de verlo

for( i  in   c( "a", "b", "c", "d" )  )
{

  cat( i, " " )

  for( j in 1:5 )
  {
    cat( j,  " " )
  }

  cat("\n" )
}


#------------------------------------------------------------------------------
# ifelse

#quiero unir las clases BAJA+1 y BAJA+2  en una nueva clase  "BAJA+2+1"
#a continuacion TRES forma de hacerlo

#primera opcion, la forma que lo haciamos en  kinder 2
#borro y libero memoria
rm( list=ls() )
gc()

#set working directory
setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

t0       <-  Sys.time()
dataset[   , clase_binaria2 := clase_ternaria ]
dataset[ clase_ternaria=="BAJA+1" | clase_ternaria=="BAJA+2", clase_binaria2 := "BAJA+2+1" ]
t1       <-  Sys.time()
as.numeric(t1 - t0, units = "secs")


#segunda opcion------------------------
#borro y libero memoria
rm( list=ls() )
gc()

#set working directory
setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

t0       <-  Sys.time()
dataset[   , clase_binaria2 := clase_ternaria ]
dataset[ clase_ternaria  %in% c( "BAJA+1", "BAJA+2"), clase_binaria2 := "BAJA+2+1" ]
t1       <-  Sys.time()
as.numeric(t1 - t0, units = "secs")


#tercera opcion, ahora, lo hago con  ifelse
#borro y libero memoria
rm( list=ls() )
gc()

setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

t0       <-  Sys.time()
dataset[   , clase_binaria2 := ifelse( clase_ternaria=="CONTINUA", "CONTINUA", "BAJA+2+1") ]
t1       <-  Sys.time()
as.numeric(t1 - t0, units = "secs")



#------------------------------------------------------------------------------
#sentencia if
#primera funcion

#quiero eliminar una columna si TODOS los valores son iguales, ya que no aporta informacion
#ATENCION  si hay un solo valor y ademas hay NULOS, entonces NO LA PUEDO ELIMINAR  

#borro y libero memoria
rm( list=ls() )
gc()

#cargo el dataset
setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

dataset$tpaquete1

if(  ( max( dataset[ ,tpaquete1], na.rm=TRUE ) == min( dataset[ ,tpaquete1], na.rm=TRUE) & 0==sum( dataset[ , is.na(tpaquete1)] ))  |
     ( nrow( dataset ) == sum( dataset[ , is.na(tpaquete1)] ) )
  ) {
  dataset[ , tpaquete1:=NULL ]
}

dataset$tpaquete1

#notar que aqui no puedo usar la sentencia  ifelse, sino la estructura de control if

#ahora lo hago con una funcion
#es la primer funcion que defino en el Kindergarten
#notar que tambien es la primera vez que parametrizo
#quiero que la funcion me sirva para cualquier nombre de campo

#En el primer intento de hacer una fucion, hago esto :

borrar_campo_sininfo <- function( pdataset, pcampo )
{
  if(  ( max( pdataset[ ,pcampo], na.rm=TRUE ) == min( pdataset[ ,pcampo], na.rm=TRUE) & 0==sum( pdataset[ , is.na(pcampo)] ))  |
       ( nrow( pdataset ) == sum( pdataset[ , is.na(pcampo)] ) )
    ) {
    pdataset[ , pcampo:=NULL ]
  }

}

#ahora pruebo invocar a la funcion y ver si hace lo que quiero

#cargo el dataset
setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

#llamo a la funcion
borrar_campo_sininfo(  dataset, "tpaquete1" )


#dio ERROR  !
#como puede ser que existan los errores en la programacion ?
#realmente  hay que estar atento a los detalles en la programacion ?

#ahora escribo la funcion que SI parametriza bien
borrar_campo_sininfo <- function( pdataset, pcampo )
{
  if(  ( max( pdataset[ ,get(pcampo)], na.rm=TRUE ) == min( pdataset[ ,get(pcampo)], na.rm=TRUE) & 0==sum( pdataset[ , is.na(get(pcampo))] ))  |
       ( nrow( pdataset ) == sum( pdataset[ , is.na(get(pcampo))] ) )
    ) {
    pdataset[ , (pcampo):=NULL ]
  }
}

#llamo a la funcion
borrar_campo_sininfo(  dataset, "tpaquete1" )
dataset$tpaquete1

#la llamo con otra columna que NO deberia borrar 
borrar_campo_sininfo(  dataset, "mcuentas_saldo" )
dataset$mcuentas_saldo

#funciono correctamente,  NO borro esa columna !

#------------------------------------------------------------------------------
#Ahora, quiero BORRAR todos los campos del dataset que no aportan informacion
#opcion 1 , un rustico  loop

#borro y libero memoria
rm( list=ls() )
gc()

#cargo el dataset
setwd( "M:\\datasets\\" )
dataset  <-  fread("201902.txt")

#defino la funcion
borrar_campo_sininfo <- function( pdataset, pcampo )
{
  if(  ( max( pdataset[ ,get(pcampo)], na.rm=TRUE ) == min( pdataset[ ,get(pcampo)], na.rm=TRUE) & 0==sum( pdataset[ , is.na(get(pcampo))] ))  |
       ( nrow( pdataset ) == sum( pdataset[ , is.na(get(pcampo))] ) )
    ) {
    pdataset[ , (pcampo):=NULL ]
    return( paste0( "-", pcampo) )
  } else {
    return( paste0( "+", pcampo) )
  }
}

ncol( dataset )

columnas <- unique( colnames( dataset ) )
for( vcol in  columnas )
{
  res <- borrar_campo_sininfo( dataset, vcol ) 
}

ncol( dataset )

#------------------------------------------------------------------------------
#Ahora, quiero BORRAR todos los campos del dataset que no aportan informacion
#opcion 2, metodo funcional de la familia apply


#borro y libero memoria
rm( list=ls() )
gc()

#cargo el dataset
setwd( "M:\\datasets\\" )
dataset  <-  fread("201904.txt")

#defino la funcion
borrar_campo_sininfo <- function( pcampo, pdataset )
{
  if(  ( max( pdataset[ ,get(pcampo)], na.rm=TRUE ) == min( pdataset[ ,get(pcampo)], na.rm=TRUE) & 0==sum( pdataset[ , is.na(get(pcampo))] ))  |
       ( nrow( pdataset ) == sum( pdataset[ , is.na(get(pcampo))] ) )
    ) {
    pdataset[ , (pcampo):=NULL ]
    return( list( campo=paste0( "-", pcampo )) )
  }  
}

ncol( dataset )

columnas <- unique( colnames(dataset) )
res <- sapply( columnas, borrar_campo_sininfo, dataset )

ncol( dataset )

#analizar lo que esta sucediendo

#------------------------------------------------------------------------------


