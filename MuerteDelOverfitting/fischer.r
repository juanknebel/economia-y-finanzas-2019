#En construccion
#Es tremendamente optimizable


bondad_corte <- function( POS, NEG, pos, neg )
{
   return( fisher.test( matrix(c(pos,neg,POS-pos,NEG-neg), ncol=2) )$p.value )
}


bondad_corte_mc <- function( POS, NEG, pos, neg, variables )
{
   f <-  fisher.test( matrix(c(pos,neg,POS-pos,NEG-neg), ncol=2), alternative="greater" )$p.value 
   
   return(  1 - (1-f)^ variables )
}

POS <-  44
NEG <- 293

x <-  c( 0 )
y <-  c( 0 )

for( n in 0:NEG )
{
  pos_diagonal <- round( (n/NEG)*POS)
  prob_previa <- 1.0
  
  for( p in  pos_diagonal:POS )
  {
    prob <- bondad_corte_mc( POS, NEG, p, n, 150)
    if( prob < 0.05 & prob_previa > 0.05 )  
    {
      x <- c( x, n )
      y <- c( y, p )
    }

    prob_previa <- prob
  }
  if( prob_previa > 0.05 ) 
  {
    x <- c( x, n )
    y <- c( y, POS )

  }
}


titulo <- paste0( "ROC Curve teorica", "pos=", POS, "  neg=", NEG, "  variables=150  alpha=0.05  " )
setwd( "M:\\work\\" )
png(file= paste0("fisher.png"), width = 6, height = 6, units = 'in', res = 300 )

#grafico
plot( x, 
      y,       
      type="n",
      main= titulo,
      xlab="neg", 
      ylab="pos", 
      panel.first = grid(NULL, lty = 1, lwd = 2),
      pch=19)

lines( x, y,   type="l" , col="red", lwd=2)  

#la diagonal
azar_neg <- c( 0, NEG )
azar_pos <- c( 0, POS )
lines( azar_neg, azar_pos,   type="l" , col="black", lwd=2)

dev.off()

