#Feature Engineering
#creo TENDENCIA, MAX, MIN  en funcion de los ultimos 6 meses historia para cada variable

#por eficiencia, la parte critica esta escrita en lenguaje C  
#ya que R es insoportablemente lento
#alguna vez se le tenia que poner freno a la demencia R o Python



#source( "~/cloud/cloud1/R/FeatureEngineering/fe_historia_fast.r" )


#limpio la memoria
rm( list=ls() )
gc()



library( "data.table" )
library( "Rcpp")



kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"
karchivo_entrada_zip     <-  "paquete_premium.zip" 
kextension               <-  "hist"

kcampos_separador        <-  "\t"
kcampo_id                <-  "numero_de_cliente"
kcampo_foto              <-  "foto_mes"
kclase_nomcampo          <-  "clase_ternaria"



kcampos_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )
ventana_regresion   <- 6


#La salida
karchivo_salida_completo <-  "paquete_premium_hist.txt"
karchivo_salida_prefijo  <-  "./hist/"
karchivo_salida_sufijo   <-  "_hist.txt"



#-------------------------------------------------

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 3*n );
  

  for(int i = 0; i < n; i++) 
  {
   
    int  libre    = 0 ;
    int  xvalor   = 1 ;
 
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;


       if( !R_IsNA( a ) ) 
       {
           y[ libre ]= a ;
       x[ libre ]= xvalor ;
          
       libre++ ;
       }
     
       xvalor++ ;
    }

 
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {   
    double  xsum  = x[0] ;
        double  ysum  = y[0] ;
        double  xysum = xsum * ysum ;
        double  xxsum = xsum * xsum ; 
        double  vmin  = y[0] ;
        double  vmax  = y[0] ;

    
        for( int h=1; h<libre; h++) 
    { 
       xsum  += x[h] ; 
       ysum  += y[h] ; 
       xysum += x[h]*y[h] ;
       xxsum += x[h]*x[h] ;
 
           if( y[h] < vmin )  vmin = y[h] ;
           if( y[h] > vmax )  vmax = y[h] ;

    }

        out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ; 
        out[ i + n ]    =  vmin ;
        out[ i + 2*n ]  =  vmax ;

    }
    else  
    { 
       out[i] = NA_REAL ; 
       out[ i + n ]    =  NA_REAL ;
       out[ i + 2*n ]  =  NA_REAL ;

    }

  }


  return out;
}')

#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201804 se interpreta como la fecha "20180501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#-------------------------------------------------
#guarda el archivo de un mes

fguardar_foto  <- function( pfoto_mes, pdataset )
{
  
  archivo_salida_mes <- paste( karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="" )

  fwrite(  dataset[ get(kcampo_foto) == pfoto_mes, ], file=archivo_salida_mes , sep=kcampos_separador, na="", row.names=FALSE ) 
}
#------------------------------------------------------

t0   <-  Sys.time()


setwd( kcarpeta_datasetsOri )
#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread(  cmd=paste( "gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador ) 


setorder( dataset, numero_de_cliente, foto_mes )


nrow( dataset )
ncol( dataset )



#----------
#paso los campos fecha a dias relativos

dataset[  , Master_Fvencimiento    := fdias_entre( get(kcampo_foto), Master_Fvencimiento )  ]
dataset[  , Master_Finiciomora     := fdias_entre( get(kcampo_foto), Master_Finiciomora )   ]
dataset[  , Master_fultimo_cierre  := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[  , Master_fechaalta       := fdias_entre( get(kcampo_foto), Master_fechaalta )     ]
dataset[  , Visa_Fvencimiento      := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )    ]
dataset[  , Visa_Finiciomora       := fdias_entre( get(kcampo_foto), Visa_Finiciomora )     ]
dataset[  , Visa_fultimo_cierre    := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )  ]
dataset[  , Visa_fechaalta         := fdias_entre( get(kcampo_foto), Visa_fechaalta )       ]



last <- nrow( dataset )
campo_id_idx  <-  match( kcampo_id, names(dataset) )
#----------


#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  campo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
#----------



#agrego al dataset las TENDENCIAS
campos_a_procesar  <- setdiff( names(dataset) ,  kcampos_no_procesar  )  

for(  campo  in  campos_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ 1:last ]  ]
   dataset[ , paste( campo, "__min", sep="" ):= nueva_col[ (last+1):(2*last) ]  ]
   dataset[ , paste( campo, "__max", sep="" ):= nueva_col[ (2*last+1):(3*last) ]   ]

   
}  



#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , kclase_nomcampo ) , kclase_nomcampo )
setcolorder( dataset, nuevo_orden )
colnames(dataset)


#grabo el archivo completo
setwd( kcarpeta_datasets )
fwrite( dataset, file=karchivo_salida_completo, sep=kcampos_separador, na="", row.names=FALSE )
#comprimo el archivo con gzip
system(  paste("gzip -f", karchivo_salida_completo) )


#creo la carpeta donde van los resultados
dir.create(file.path(kcarpeta_datasets, kextension), showWarnings = FALSE)


#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   unique( dataset[ , get(kcampo_foto) ] ) 

#genero un archivo para cada foto
lapply(  fotos_distintas,  fguardar_foto,  pdataset=dataset ) 

t1   <-  Sys.time()
tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")



#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


