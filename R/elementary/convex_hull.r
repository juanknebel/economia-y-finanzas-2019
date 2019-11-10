
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

graficar_init  = function( pdataset, parchivo, ptitulo, archivo )
{
  #calculos basicos
  universo  <-  nrow(pdataset )
  pos_total <-  sum(pdataset$clase01 )
  neg_total <-  universo -  pos_total

  #la diagonal
  azar_neg <- c( 0, neg_total )
  azar_pos <- c( 0, pos_total )

  if( archivo )
  {
    setwd( "M:\\work\\" )
    png(file= paste0(parchivo, ".png") , width = 6, height = 6, units = 'in', res = 300 )
  }
  
  #grafico
  plot( azar_neg, 
        azar_pos,       
        type="n",
        main= ptitulo,
        xlab="neg", 
        ylab="pos", 
        panel.first = grid(NULL, lty = 1, lwd = 2),
        pch=19)

  lines( azar_neg, azar_pos,   type="l" , col="black", lwd=2)
  
}
#----------------------

graficar_diagonal  = function( pdataset )
{
  #calculos basicos
  universo  <-  nrow(pdataset )
  pos_total <-  sum(pdataset$clase01 )
  neg_total <-  universo -  pos_total

  #la diagonal
  azar_neg <- c( 0, neg_total )
  azar_pos <- c( 0, pos_total )

  lines( azar_neg, azar_pos,   type="l" , col="black", lwd=4)
  
  #lines( c(0, neg_total*0.9), c(pos_total*0.10, pos_total),   type="l" , col="black", lwd=2)
  #lines( c(neg_total*0.1, neg_total), c(0, pos_total*0.9),   type="l" , col="black", lwd=2)
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


#----------------------
#Concepto fundamental
#se ordena el dataset por una variable
#y se lleva el conteo de positivos y negativos

columna_graficar  <-  function(pcolumna, pdataset )
{
  #calculos basicos
  universo  <-  nrow(pdataset )
  pos_total <-  sum(pdataset$clase01 )
  neg_total <-  universo -  pos_total

  #ordeno por  <pcolumna, azar>
  #dentro de los registros que tienen el mismo valor de pcolumna, ordeno por el campo que invente llamado  azar
  #los NA de pcolumna  van al inicio del orden
  univar <- pdataset[ order(get(pcolumna), na.last=FALSE, azar),   c("clase01", pcolumna), with=FALSE]

  #acumulo positivos y negativos,  operacion vectorial
  neg_acum  <- cumsum( 1- univar$clase01 )
  pos_acum  <- cumsum( univar$clase01 )
  
  for( i in 1:universo )
  {
     if(  pos_acum[i]/(pos_acum[i]+neg_acum[i])   <   pos_total/(pos_total+neg_total) )
     {
       pos_acum[i] <- pos_total -  pos_acum[i]
       neg_acum[i] <- neg_total -  neg_acum[i]
     }
  }
 
  #dibujo la curva, que esta compuesta de miles de puntos
  #lines( neg_acum, pos_acum,   type="l" , col="red", lwd=2)
  points( neg_acum, pos_acum, type="p", col="red", pch=20 )

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

dataset_graficar <-  function( pdataset )
{
 vcol <- colnames( pdataset )
 vcol <-  setdiff( vcol, c("clase_ternaria", "clase01","azar","numero_de_cliente" ) )
 
 lapply( vcol, columna_graficar, pdataset )
}
#----------------------
#La randomizacion de libro

clase_original <-  dataset$clase01

dataset_sub <-  dataset[ mcaja_ahorro_Paquete<241.04 &  mtarjeta_visa_consumo<162.03 & mdescubierto_preacordado<0.585 & mcuentas_saldo< -872.43, ]

dataset_sub <-  dataset[ (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) & mcuentas_saldo< -8228.3 & marketing_coss_selling>=6 & mcomisiones_mantenimiento>=684.79, ]
dataset_sub <-  dataset[ (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) & mcuentas_saldo< -8228.3 & marketing_coss_selling>=6, ]

#dataset_sub <-  data.table::copy(dataset[ mcuentas_saldo <= -120000, ])
#dataset_sub <-  dataset

setwd( "M:\\work\\" )
pos <- sum(dataset_sub$clase01)
neg <- nrow( dataset_sub ) - pos
permutaciones <- 20 

titulo <-  paste0( "ROC Curve  pos=", pos,  "   neg=", neg, "  permutaciones=", permutaciones) 
graficar_init( dataset_sub , "ROC2_randomizar", titulo,  archivo=FALSE )
for( i in 1:permutaciones )
{
  dataset_sub[   , clase01 := sample( dataset_sub$clase01, replace=FALSE ) ]
  
  dataset_graficar( dataset_sub  )
}


graficar_diagonal( dataset_sub )

dev.off()

#----------------------

#Los canaritos de libro

clase_original <-  dataset$clase01

dataset_sub <-  dataset[ mcaja_ahorro_Paquete<241.04 &  mtarjeta_visa_consumo<162.03 & mdescubierto_preacordado<0.585 & mcuentas_saldo< -872.43, ]
nrow(  dataset[ (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) & mcuentas_saldo< -8228.3 & marketing_coss_selling>=6 , ] )

dataset_sub <-  dataset[ (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) & mcuentas_saldo< -8228.3 & marketing_coss_selling>=6 & mcomisiones_mantenimiento>=684.79, ]
dataset_sub <-  dataset[ (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) & mcuentas_saldo< -8228.3 & marketing_coss_selling>=6, ]

dataset_sub <-  dataset_sub[ , c("clase_ternaria", "clase01","azar","numero_de_cliente" )]
largo <- nrow( dataset_sub )

pos <- sum(dataset_sub$clase01)
neg <- nrow( dataset_sub ) - pos
num_canaritos <- 1000

for( canarito in 1:num_canaritos )  dataset_sub[  , paste0( "canarito", canarito) :=  runif(largo ) ]


setwd( "M:\\work\\" )

titulo <-  paste0( "ROC Curve  pos=", pos,  "   neg=", neg, "  canaritos=", num_canaritos) 
graficar_init( dataset_sub , "ROC2_canaritos_1000", titulo,  archivo=FALSE )

dataset_graficar( dataset_sub  )
graficar_diagonal( dataset_sub )

dev.off()


#----------------------


clase_original <-  dataset$clase01

dataset_sub <-  dataset[ mcaja_ahorro_Paquete<241.04 &  mtarjeta_visa_consumo<162.03 & mdescubierto_preacordado<0.585 & mcuentas_saldo< -872.43, ]
#dataset_sub <-  data.table::copy( dataset[ mcuentas_saldo <= -120000, ] )
#dataset_sub <-  dataset



graficar_init( "prueba",  tope  )
for( i in 1:ncol(dataset) )
{
  dataset_sub[  , canarito_universal :=  runif(nrow(dataset_sub)) ]  
  columna_graficar(  "canarito_universal", dataset_sub )
}




for(  tope in  c( 10, 50, 100, 500, 1000, 1500, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000 ) )
{

  graficar_init( dataset_sub, paste0( "ROC2_canaritos_",  tope ) )

  for( i in 1:tope )
  {
    dataset_sub[  , canarito_universal :=  runif(nrow(dataset_sub)) ]  
    columna_graficar(  "canarito_universal", dataset_sub )
  }

  graficar_diagonal( dataset_sub )
  dev.off()
}



for(  tope in  c( 100 ) )
{

  graficar_init( dataset_sub, paste0( "ROC2_canaritos_",  tope ) )

  for( i in 1:tope )
  {
    dataset_sub[  , canarito_universal :=  runif(nrow(dataset_sub)) ]  
    columna_graficar(  "canarito_universal", dataset_sub )
  }

  graficar_diagonal( dataset_sub )
  dev.off()
}


