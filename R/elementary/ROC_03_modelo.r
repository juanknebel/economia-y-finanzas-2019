#En construccion



library("data.table")


#cargo los datos
dataset <- fread("M:\\datasets\\201902.txt")

#creo una clase que se 1 cuando es BAJA+2   , y  0 en caso contrario
#esto me simplifica las cuentas
dataset[ , clase01:= as.numeric(clase_ternaria=="BAJA+2") ]


#creo una variable azar que me va a ser util

#inicializo el generador de numeros aleatorios
set.seed( 102191 )
dataset[ , azar   := runif(nrow(dataset)) ]

set.seed( 33 )
dataset[ , azar2   := runif(nrow(dataset)) ]


#calculos basicos
universo  <-  nrow(dataset )
pos_total <-  sum(dataset$clase01 )
neg_total <-  universo -  pos_total


#Los primeros son los = Na's
#La  gran linea que luego cruza el azar es  = 10
#La ultima pequeña linea que sube hasta el (1,1)  son los  { 11, 12, 19 }

#----------------------
pcolumna <- "mcuentas_saldo"

columna_metricas  = function(pcolumna, dataset, dataset_test)
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  pos_na    <-  sum(  dataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE )
  neg_na    <-  sum(  1- dataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE  )

  gan_na    <-  19500*pos_na  - 500*neg_na
  

  #ordeno creciente por  <pcolumna, azar>
  univar <- dataset[ !is.na(get(pcolumna)), .(clase01, azar, get(pcolumna)) ] 
  setorderv( univar, cols=c("V3", "azar") )
  
  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  gan_acum  <- 19500*pos_acum - 500*neg_acum

  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)
  AUC_creciente_max <-  max( AUC_vector)
  gan_creciente_max <-  max( gan_acum )
  corte <-  univar[ which.max(gan_acum ), V3 ]
  
  
  gan_creciente_max_test <-  sum(  dataset_test[  get(pcolumna) <= corte,   ifelse( clase01, 19500, -500) ] )
  
  

  #ordeno DEcreciente por  <pcolumna, azar>
  setorderv( univar, cols=c("V3", "azar"), order=c(-1,1) )
  

  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )

  gan_acum  <- 19500*pos_acum - 500*neg_acum
 
  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)
  AUC_decreciente_max <-  max( AUC_vector)
  gan_decreciente_max <-  max( gan_acum )
  gan_decreciente_max_test <-  sum(  dataset_test[  get(pcolumna) > corte,   ifelse( clase01, 19500, -500) ] )

  test_pos_na    <-  sum(  dataset_test[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE )
  test_neg_na    <-  sum(  1- dataset_test[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE  )

  test_gan_na    <-  19500*test_pos_na  - 500*test_neg_na



  return(  list(  "columna"  = pcolumna,  
                  "AUC_max"  = pmax( AUC_creciente_max, AUC_decreciente_max) ,
                  "gan_max"  = pmax( gan_creciente_max, gan_decreciente_max) + pmax( gan_na, 0 ),
                  "gan_max_test" =  pmax( gan_creciente_max_test, gan_decreciente_max_test ) + pmax( test_gan_na, 0 )
               )  
        ) 
 
}
#----------------------

dataset_test <- dataset[  azar2>  0.5 , ]
dataset      <- dataset[  azar2<= 0.5 , ]


columna_metricas( "mcuentas_saldo",  dataset, dataset_test )


metricas <- lapply( colnames( dataset) , columna_metricas,  dataset, dataset_test )

metricas <- rbindlist( metricas )

metricas <- metricas[ order( -AUC_max ), ]
metricas[ 1:10, ]


metricas <- metricas[ order( -gan_max ), ]
metricas[ 1:30, ]



columna_metricas( "ttarjeta_visa",  dataset )


#------------------------------------------


columna_graficar_ganancia_n  = function(dataset, pcolumna, pcantidad )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  #ordeno por  <pcolumna, azar>
  univar <- dataset[ order(get(pcolumna), na.last=FALSE, azar),   c("clase01", pcolumna), with=FALSE]

  #acumulo positivos y negativos,  operacion vectorial
  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  gan_acum  <- 19500*pos_acum - 500*neg_acum

  #grafico
  plot( seq(pcantidad), 
        gan_acum[1:pcantidad],       
        type="n",
        main=paste( "Ganancia ordenado por", pcolumna  ),
        xlab="registros", 
        ylab="Ganancia", 
        pch=19)

  lines( seq(pcantidad), gan_acum[1:pcantidad],   type="l" , col="blue", lwd=2)
 

  return(  list( "variable"= pcolumna, 
                 "valor"   = univar[ which.max( gan_acum ),  get(pcolumna)],
                 "gan_max" = max( gan_acum),
                 "regis"   = which.max( gan_acum )
               ) 
         ) 
}
#---------------------

columna_graficar_ganancia_n( dataset, "tmovimientos_ultimos90dias", 35000  )


#Y ahora la tabla de contingencia

ftable(dataset[ tmovimientos_ultimos90dias <= 20, clase_ternaria])
ftable(dataset[ tmovimientos_ultimos90dias  > 20, clase_ternaria])


