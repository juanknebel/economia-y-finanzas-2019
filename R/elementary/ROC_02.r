#Objetivo:  dibujar la "curva ROC de una variable"

#limpio la memoria
rm(list=ls())
gc()


library("data.table")


#cargo los datos
dataset <- fread("M:\\datasets\\201902.txt")

#creo una clase que se 1 cuando es BAJA+2   , y  0 en caso contrario
#esto me simplifica las cuentas
dataset[ , clase01:= as.numeric(clase_ternaria=="BAJA+2") ]


#creo una variable azar que me va a ser util
#para ordenar al azar los registros que tienen el mismo valor para un campo
#asi el dibujo de la curva ROC de ese segmento es una recta
dataset[ , azar   := runif(nrow(dataset)) ]


#calculos basicos
universo  <-  nrow(dataset )
pos_total <-  sum(dataset$clase01 )
neg_total <-  universo -  pos_total

#Creo dos funciones de forma de poder superponer varios cortes de una misma variable

#-------------------------------------------------------------

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

pred_graficar  = function(dataset, pcolumna, pvalor )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  pos_pred <- sum( dataset[ get(pcolumna) <= pvalor , clase01] )
  neg_pred <- sum( 1 - dataset[ get(pcolumna) <= pvalor, clase01] )

  AUC <- (pos_pred*neg_pred + (pos_pred + pos_total)*(neg_total-neg_pred) ) / (2*pos_total*neg_total)

  #creo el vector con los tres puntos
  vneg <- c( 0, neg_pred,  neg_total )
  vpos <- c( 0, pos_pred,  pos_total )

  #grafico
  lines( vneg, vpos,   type="l" , col="blue", lwd=2)

  return( AUC )
}
#----------------------

graficar_init()

#ejecutar las instrucciones de a una y ver el efecto
pred_graficar( dataset, "mcuentas_saldo", -100000 )
pred_graficar( dataset, "mcuentas_saldo",  -10000 )
pred_graficar( dataset, "mcuentas_saldo",   -2000 )
pred_graficar( dataset, "mcuentas_saldo",       0 )
pred_graficar( dataset, "mcuentas_saldo",   10000 )
pred_graficar( dataset, "mcuentas_saldo",   50000 )
pred_graficar( dataset, "mcuentas_saldo",  100000 )



#----------------------
#Concepto fundamental
#se ordena el dataset por una variable
#y se lleva el conteo de positivos y negativos

columna_graficar  = function(dataset, pcolumna )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  #ordeno por  <pcolumna, azar>
  #dentro de los registros que tienen el mismo valor de pcolumna, ordeno por el campo que invente llamado  azar
  #los NA de pcolumna  van al inicio del orden
  univar <- dataset[ order(get(pcolumna), na.last=FALSE, azar),   c("clase01", pcolumna), with=FALSE]

  #acumulo positivos y negativos,  operacion vectorial
  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  #dibujo la curva, que esta compuesta de miles de puntos
  lines( neg_acum, pos_acum,   type="l" , col="red", lwd=2)

  #calculo el vector de cada AUC, que consiste en cortar exactamente en ese punto
  AUC_vector <-  ( pos_acum*neg_acum + (pos_acum+pos_total)*(neg_total-neg_acum) ) / (2*pos_total*neg_total)

  #voy a calcular cual es el corte que genera la mayor AUC

  return(  list( "variable"= pcolumna, 
                 "valor"   = univar[ which.max( AUC_vector ),  get(pcolumna)],
                 "AUC_max" = max( AUC_vector)
               ) 
        ) 
 
}
#----------------------

columna_graficar(  dataset, "mcuentas_saldo"  )




#ahora uso el valor optimo que encontre en la instruccion anterior 
#cortar mcuentas_saldo en 1275.59

graficar_init()

pred_graficar( dataset, "mcuentas_saldo", 1275.59 )

columna_graficar(  dataset, "mcuentas_saldo"  )


#como da la tabla de contingencia

ftable(dataset[ mcuentas_saldo <= 1275.59, clase_ternaria])
ftable(dataset[ mcuentas_saldo >  1275.59, clase_ternaria])

#-----------------------
#Desvio,  calcular la ganancia acumulada



columna_graficar_ganancia  = function(dataset, pcolumna )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  #ordeno por  <pcolumna, azar>
  #dentro de los registros que tienen el mismo valor de pcolumna, ordeno por el campo que invente llamado  azar
  #los NA de pcolumna  van al inicio del orden
  univar <- dataset[ order(get(pcolumna), na.last=FALSE, azar),   c("clase01", pcolumna), with=FALSE]

  #acumulo positivos y negativos,  operacion vectorial
  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
 
  gan_acum  <- 19500*pos_acum - 500*neg_acum

  #grafico
  plot( seq(universo), 
        gan_acum,       
        type="n",
        main=paste( "Ganancia ordenado por", pcolumna  ),
        xlab="registros", 
        ylab="Ganancia", 
        pch=19)

  lines( seq(universo), gan_acum,   type="l" , col="blue", lwd=2)
 

  return(  list( "variable"= pcolumna, 
                 "valor"   = univar[ which.max( gan_acum ),  get(pcolumna)],
                 "gan_max" = max( gan_acum),
                 "regis"   = which.max( gan_acum )
               ) 
         ) 
}
#---------------------

columna_graficar_ganancia( dataset, "mcuentas_saldo"  )


# Y ahora graficando los primeros  n registros

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

columna_graficar_ganancia_n( dataset, "mcuentas_saldo", 20000  )
