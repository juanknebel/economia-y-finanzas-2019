#Objetivo1:  resolver el tema de los NAs  y si se debe ordenar ascendente o descendente
#Objetivo2:  aplicar a todas las variables


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


#calculos basicos
universo  <-  nrow(dataset )
pos_total <-  sum(dataset$clase01 )
neg_total <-  universo -  pos_total

#-------------------------------------------------------------
# Creo una funcion para automatizar
graficar_init  = function()
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  #la diagonal
  azar_neg <- c( 0, neg_total )
  azar_pos <- c( 0, pos_total )

  #grafico
  plot( azar_neg, 
        azar_pos,       
        type="n",
        main=paste( "ROC Curve" ),
        xlab="neg", 
        ylab="pos", 
        pch=19)

  lines( azar_neg, azar_pos,   type="l" , col="black", lwd=2)
}
#----------------------

columna_graficar  = function(dataset, pcolumna )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  #ordeno por  <pcolumna, azar>
  univar <- dataset[ order(get(pcolumna), na.last=FALSE, azar),   c("clase01", pcolumna), with=FALSE]

  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  lines( neg_acum, pos_acum,   type="l" , col="red", lwd=2)

  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)

  return(  list( "variable"= pcolumna, 
                 "valor"   = univar[ which.max( AUC_vector ),  get(pcolumna)],
                 "AUC_max" = max( AUC_vector)
               ) ) 
 
}
#----------------------

graficar_init()
columna_graficar(  dataset, "Visa_cuenta_estado"  )

#Los primeros son los = Na's
#La  gran linea que luego cruza el azar es  = 10
#La ultima pequeña linea que sube hasta el (1,1)  son los  { 11, 12, 19 }



columna_metricas  = function(pcolumna, dataset)
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  pos_na    <-  sum(  dataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE )
  neg_na    <-  sum(  1- dataset[ is.na( get(pcolumna) ), clase01 ],  na.rm=TRUE  )

  va_ultimo <-  pos_na/( pos_na + neg_na + 1 )   <   pos_total/neg_total

  #ordeno creciente por  <pcolumna, azar>
  univar <- dataset[ order(get(pcolumna), na.last=va_ultimo, azar),   c("clase01", pcolumna), with=FALSE]

  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  gan_acum  <- 19500*pos_acum - 500*neg_acum

  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)
  AUC_creciente_max <-  max( AUC_vector)
  gan_creciente_max <-  max( gan_acum )

  #ordeno DEcreciente por  <pcolumna, azar>
  univar <- dataset[ order(-get(pcolumna), na.last=va_ultimo, azar),   c("clase01", pcolumna), with=FALSE]

  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )

  gan_acum  <- 19500*pos_acum - 500*neg_acum
 
  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)
  AUC_decreciente_max <-  max( AUC_vector)
  gan_decreciente_max <-  max( gan_acum )


  return(  list(  "columna"  = pcolumna,  
                  "AUC_max"  = pmax( AUC_creciente_max, AUC_decreciente_max) ,
                  "gan_max"  = pmax( gan_creciente_max, gan_decreciente_max) 
               )  
        ) 
 
}
#----------------------

columna_metricas( "mcuentas_saldo",  dataset )


metricas <- lapply( colnames( dataset) , columna_metricas,  dataset )

metricas <- rbindlist( metricas )

metricas <- metricas[ order( -AUC_max ), ]
metricas[ 1:10, ]


metricas <- metricas[ order( -gan_max ), ]
metricas[ 1:10, ]



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


