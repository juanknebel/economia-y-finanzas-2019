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

dataset_actual <- dataset_actual[  (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) )  , ]
dataset_futuro <- dataset_futuro[  (Visa_mfinanciacion_limite<167.9 | is.na(Visa_mfinanciacion_limite<167.9) ) , ]


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


for( vcolumna in res$columna )
{
 cat( vcolumna, "\n" ) 
#vcolumna <- "mcuentas_saldo"

setorderv( dataset_actual, c( vcolumna, "azar")  )
setorderv( dataset_futuro, c( vcolumna, "azar")  )

frollmean( dataset_actual$clase01, 100 )

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
  