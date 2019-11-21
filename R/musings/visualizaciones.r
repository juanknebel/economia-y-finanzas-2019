rm(list=ls())
gc()


library("data.table")

#------------------------------------------------------------------------------

liftcurve_area  <-  function(  pcolumna, pdataset )
{
  tb_par <-  pdataset[  , c(pcolumna, "clase01"), with=FALSE ]
  tb_par[  , azar2:= runif( nrow(tb_par)) ]
  
  setorderv( tb_par, c( pcolumna, "azar2"), c( 1,1), na.last=FALSE  )
  area1 <-  sum( cumsum( tb_par$clase01 ) )
  
  setorderv( tb_par, c( pcolumna, "azar2"), c( 1,1), na.last=TRUE  )
  area2 <-  sum( cumsum( tb_par$clase01 ) )
  
  setorderv( tb_par, c( pcolumna, "azar2"), c(-1,1), na.last=FALSE  )
  area3 <-  sum( cumsum( tb_par$clase01 ) )
  
  setorderv( tb_par, c( pcolumna, "azar2"), c(-1,1), na.last=TRUE  )
  area4 <-  sum( cumsum( tb_par$clase01 ) )
  
  mejor_area <- pmax( area1, area2, area3, area4)
  
  return(  list("columna"=pcolumna, "area"= mejor_area ))
}
#------------------------------------------------------------------------------


setwd( "M:\\datasets\\dias" )
dataset_actual <- fread( "201902_dias.txt" )
dataset_futuro <- fread( "201904_dias.txt" )

setwd( "/media/Shared/gustavo/cloud1/datasets/" )

dataset <- fread( "paquete_premium_hist.txt.gz" )

dataset_actual <- dataset[ foto_mes==201902, ]
dataset_futuro <- dataset[ foto_mes==201904, ]


dataset_actual <- dataset_actual[  (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) )  , ]
dataset_futuro <- dataset_futuro[  (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) , ]
rm( dataset )
gc()


dataset_actual[  , clase01:= as.numeric( clase_ternaria=="BAJA+2" ) ]
dataset_futuro[  , clase01:= as.numeric( clase_ternaria=="BAJA+2" ) ]

dataset_actual[  , ganancia:= ifelse( clase01==1, 19500, -500 ) ]
dataset_futuro[  , ganancia:= ifelse( clase01==1, 19500, -500 ) ]

dataset_actual[  , azar:= runif(nrow(dataset_actual)) ]
dataset_futuro[  , azar:= runif(nrow(dataset_futuro)) ]

#Calculo un indicador de las columnas
colbuenas <- setdiff( colnames( dataset_actual ) , c("clase_ternaria","azar","clase01","ganancia") )
res <- lapply( colbuenas, liftcurve_area, dataset_actual )
res <- rbindlist( res )
setorder(  res,  -area )

corte <- 10

setorderv( dataset_actual, c( "mcuentas_saldo", "azar")  )

vacum <- cumsum( dataset_actual$clase01 )
linea_muerte <-  c( vacum[corte] )

for( i in 1:100 )
{
  dataset_actual[   , clase01 := sample( dataset_actual$clase01, replace=FALSE ) ]
  tope <- 0
  for( vcolumna in res$columna )
  {
    setorderv( dataset_actual, c( vcolumna, "azar")  )
    vacum <- cumsum( dataset_actual$clase01 )
    if(  vacum[corte] > tope )
    {  
      tope <- vacum[corte]
      cat( tope, " "  )
    }
  }
  linea_muerte <- c( linea_muerte, tope )

}


linea_muerte2 <-  c( linea_muerte[1] )

for( i in 1:100 )
{
  dataset_actual[   , clase01 := sample( dataset_actual$clase01, replace=FALSE ) ]
  tope <- 0
  for( vcolumna in res$columna )
  {
    dataset_actual[   , clase01 := sample( dataset_actual$clase01, replace=FALSE ) ]
    vacum <- cumsum( dataset_actual$clase01 )
    if(  vacum[corte] > tope )
    {  
      tope <- vacum[corte]
      cat( tope, " "  )
    }
  }
  linea_muerte2 <- c( linea_muerte2, tope )

}

lm1 <- linea_muerte[-1]
lm2 <- linea_muerte2[-1]

median( lm1 )
median( lm2 )
max( lm1 )
max( lm2 )


for( vcolumna in res$columna )
{
 cat( vcolumna, "\n" ) 
#vcolumna <- "mcuentas_saldo"

setorderv( dataset_actual, c( vcolumna, "azar")  )
setorderv( dataset_futuro, c( vcolumna, "azar")  )

frollmean( dataset_actual$clase01, 100 )

dataset_actual[  , pos_acum:= cumsum( clase01) ]

dataset_actual[  , ganancia_acum:= cumsum( ganancia) ]
dataset_futuro[  , ganancia_acum:= cumsum( ganancia) ]
ultimo <- pmin( nrow( dataset_actual), nrow(dataset_futuro) )

 #grafico
  plot( x=1:ultimo , 
        y=dataset_actual[ 1:ultimo,ganancia_acum],
        type="n",
        main= "graphic",
        xlab="regs", 
        ylab="ganancia", 
        panel.first = grid(NULL, lty = 1, lwd = 2),
        pch=19)

  lines( 1:ultimo, dataset_actual[ 1:ultimo,ganancia_acum],   type="l" , col="black", lwd=2)
  lines( 1:ultimo, dataset_futuro[ 1:ultimo,ganancia_acum],   type="l" , col="blue", lwd=2)
  
  
yactual <- frollmean( dataset_actual$clase01, align="center", 200 )
yfuturo <- frollmean( dataset_futuro$clase01, align="center", 200 )

 #grafico
  plot( x=1:ultimo , 
        y=dataset_actual[ 1:ultimo,ganancia_acum],
        type="n",
        main= vcolumna,
        xlab="regs", 
        ylab="ganancia", 
        panel.first = grid(NULL, lty = 1, lwd = 2),
        pch=19)

  lines( 1:ultimo, dataset_actual[ 1:ultimo,ganancia_acum],   type="l" , col="black", lwd=2)
  lines( 1:ultimo, dataset_futuro[ 1:ultimo,ganancia_acum],   type="l" , col="blue", lwd=2)
  
  readline(prompt="Press [enter] to continue")
  
yactual <- frollmean( dataset_actual$clase01, align="center", 200 )
yfuturo <- frollmean( dataset_futuro$clase01, align="center", 200 )


  plot( x=1:ultimo , 
        y=yactual,
        type="n",
        main= vcolumna,
        xlab="regs", 
        ylab="ganancia", 
        panel.first = grid(NULL, lty = 1, lwd = 2),
        pch=19)

  lines( 1:ultimo, yactual[1:ultimo],   type="l" , col="black", lwd=2)
  lines( 1:ultimo, yfuturo[1:ultimo],   type="l" , col="blue", lwd=2)
  
  readline(prompt="Press [enter] to continue")
}
  