#Objetivo:  introducir concepto de curva ROC

#limpio la memoria
rm(list=ls())
gc()

library("data.table")


#cargo los datos
dataset <- fread("M:\\datasets\\201902.txt")

#creo una clase que se 1 cuando es BAJA+2   , y  0 en caso contrario
#esto me simplifica las cuentas
dataset[ , clase01:= as.numeric(clase_ternaria=="BAJA+2") ]


#calculos basicos
universo  <-  nrow(dataset )
pos_total <-  sum(dataset$clase01 )
neg_total <-  universo -  pos_total

#tambien podria hacer
neg_total <-  sum( 1 - dataset$clase01 )


#----------------

#Dibujo la curva ROC del predicado    mcuentas_saldo<= -120000

#positivos del predicado
pos_pred    <- sum(  dataset[ (mcuentas_saldo<= -120000) , clase01] )

#positivos del predicado
neg_pred    <- sum(  dataset[ (mcuentas_saldo<= -120000) , 1 - clase01] )

#Tener en cuenta que los NA's  no se estan contando en este predicado !



#la diagonal de la Curva Roc
azar_neg <- c( 0, neg_total )
azar_pos <- c( 0, pos_total )

#grafico
plot( azar_neg, 
      azar_pos,       
      type="n",
      main=paste( "ROC Curve", "(mcuentas_saldo<= -120000)" ),
      xlab="neg", 
      ylab="pos", 
      pch=19)

#dibujo la diagonal
lines( azar_neg, azar_pos,   type="l" , col="black", lwd=2)


#para el predicado creo el vector con los tres puntos
vneg <- c( 0, neg_pred,  neg_total )
vpos <- c( 0, pos_pred,  pos_total )

#dibujo la linea
lines( vneg, vpos,   type="l" , col="blue", lwd=2)


#Calculo  AUC  Area Under Curve
triangulo_area <- pos_pred*neg_pred/2 
trapecio_area  <- (pos_pred + pos_total)*(neg_total-neg_pred) / 2
AUC            <- (triangulo_area + trapecio_area ) / (pos_total*neg_total)


#----------------------
# Creo una funcion para automatizar

pred_graficar  = function(dataset, pcolumna, pvalor )
{
  #calculos basicos
  universo  <-  nrow(dataset )
  pos_total <-  sum(dataset$clase01 )
  neg_total <-  universo -  pos_total

  pos_pred <- sum( dataset[ get(pcolumna) <= pvalor , clase01] )
  neg_pred <- sum( 1 - dataset[ get(pcolumna) <= pvalor, clase01] )

  AUC <- (pos_pred*neg_pred + (pos_pred + pos_total)*(neg_total-neg_pred) ) / (2*pos_total*neg_total)


  #la diagonal
  azar_neg <- c( 0, neg_total )
  azar_pos <- c( 0, pos_total )

  #grafico
  plot( azar_neg, 
        azar_pos,       
        type="n",
        main=paste( "ROC Curve", "(", pcolumna, "<=", pvalor, ")", "AUC=", AUC ),
        xlab="neg", 
        ylab="pos", 
        pch=19)

  #dibujo la linea del azar
  lines( azar_neg, azar_pos,   type="l" , col="black", lwd=2)

  #creo el vector con los tres puntos
  vneg <- c( 0, neg_pred,  neg_total )
  vpos <- c( 0, pos_pred,  pos_total )

  #dibujo la curva del predicado
  lines( vneg, vpos,   type="l" , col="blue", lwd=2)

  return( AUC )
}
#----------------------

#algunos cortes de la variable  mcuentas_saldo
pred_graficar( dataset, "mcuentas_saldo", -120000 )
pred_graficar( dataset, "mcuentas_saldo", 0 )
pred_graficar( dataset, "mcuentas_saldo", 10000 )


#una variable que no esta correlacionada con la clase
pred_graficar( dataset, "cliente_edad", 40 )


#una vieja conocida variable
pred_graficar( dataset, "Visa_cuenta_estado", 10 )

#por que esta por debajo de la linea de azar ?

