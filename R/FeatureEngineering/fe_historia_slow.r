#Feature Engineering
#creo AVG, MAX, MIN  en funcion de los ultimos 6 meses historia para cada variable


#source( "~/cloud/cloud1/R/FeatureEngineering/fe_historia.r" )


#limpio la memoria
rm( list=ls() )
gc()


install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")
library( "data.table" )



kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"
karchivo_entrada_zip     <-  "paquete_premium.zip" 
kextension               <-  "hist"

kcampos_separador        <-  "\t"
kcampo_id                <-  "numero_de_cliente"
kcampo_foto              <-  "foto_mes"
kclase_nomcampo          <-  "clase_ternaria"



kcampos_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )
kventana             <- 6


#La salida
karchivo_salida_completo <-  "paquete_premium_hist.txt"
karchivo_salida_prefijo  <-  "./hist/"
karchivo_salida_sufijo   <-  "_hist.txt"


#------------------------------------------------------------------------------

#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  <- function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------------------------------
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

#paso los campos fecha a dias relativos

dataset[  , Master_Fvencimiento    := fdias_entre( get(kcampo_foto), Master_Fvencimiento )  ]
dataset[  , Master_Finiciomora     := fdias_entre( get(kcampo_foto), Master_Finiciomora )   ]
dataset[  , Master_fultimo_cierre  := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[  , Master_fechaalta       := fdias_entre( get(kcampo_foto), Master_fechaalta )     ]
dataset[  , Visa_Fvencimiento      := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )    ]
dataset[  , Visa_Finiciomora       := fdias_entre( get(kcampo_foto), Visa_Finiciomora )     ]
dataset[  , Visa_fultimo_cierre    := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )  ]
dataset[  , Visa_fechaalta         := fdias_entre( get(kcampo_foto), Visa_fechaalta )       ]



#creo los campos nuevos
setorder( dataset, numero_de_cliente, foto_mes )

cols_trabajo <- setdiff( colnames(dataset) , kcampos_no_procesar )

#calculo el promedio de todas las columnas  cols_trabajo con la funcion  frollmean
dataset[ , paste0(cols_trabajo, "__avg") := lapply(.SD, data.table::frollmean, n = kventana, na.rm=TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_trabajo]


# Maximo
dataset[ , paste0(cols_trabajo, "__max") := lapply(.SD, data.table::frollapply, n = kventana, max, na.rm=TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_trabajo]

# Minimo
dataset[ , paste0(cols_trabajo, "__min") := lapply(.SD, data.table::frollapply, n = kventana, min, na.rm=TRUE), 
           by = numero_de_cliente, 
           .SDcols = cols_trabajo]

dataset[ 124:144 ,c("numero_de_cliente", "foto_mes", "mcuentas_saldo",  "mcuentas_saldo__min") ]




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


