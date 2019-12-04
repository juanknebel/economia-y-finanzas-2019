#Objetivo: crear variables derivadas al dataset, basadas en el mismo registro

#Feature Engineering
#creo nuevas variables dentro del mismo mes
#NO utilizo la historia
#Agrego suma de MasterCard y Visa

#Condimentar a gusto con nuevas variables


#source( "~/cloud/cloud1/R/FeatureEngineering/fe_presente.r" )



#limpio la memoria
rm( list=ls() )
gc()


library( "data.table" )


kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"

kcampos_separador        <-  "\t"
karchivo_entrada_zip     <-  "paquete_premium.zip"

kcampo_foto              <- "foto_mes"
kclase_nomcampo          <- "clase_ternaria"

karchivo_salida_completo <-  "paquete_premium_ext.txt"
kextension               <-  "ext"
karchivo_salida_prefijo  <-  paste( "./", kextension, "/",    sep="" )
karchivo_salida_sufijo   <-  paste( "_", kextension, ".txt",    sep="" )


#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------
#guarda el archivo de un mes

fguardar_foto  = function( pfoto_mes, pdataset )
{
  
  archivo_salida_mes <- paste( karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="" )

  fwrite(  dataset[ get(kcampo_foto) == pfoto_mes, ], file=archivo_salida_mes , sep=kcampos_separador, na="", row.names=FALSE ) 
}
#------------------------------------------------------

t0   <-  Sys.time()


setwd( kcarpeta_datasetsOri )

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread(  cmd=paste( "gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador ) 



nrow( dataset )
ncol( dataset )


#paso los campos fecha a dias relativos

dataset[  , Master_Fvencimiento    := fdias_entre( get(kcampo_foto), Master_Fvencimiento )  ]
dataset[  , Master_Finiciomora     := fdias_entre( get(kcampo_foto), Master_Finiciomora )   ]
dataset[  , Master_fultimo_cierre  := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[  , Master_fechaalta       := fdias_entre( get(kcampo_foto), Master_fechaalta )     ]
dataset[  , Visa_Fvencimiento      := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )    ]
dataset[  , Visa_Finiciomora       := fdias_entre( get(kcampo_foto), Visa_Finiciomora )     ]
dataset[  , Visa_fultimo_cierre    := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )  ]
dataset[  , Visa_fechaalta         := fdias_entre( get(kcampo_foto), Visa_fechaalta )       ]




#se crean los nuevos campos para Master y Visa, teniendo en cuenta los NA's

dataset[ , mv_cuenta_estado2       := pmax( Master_cuenta_estado,  Visa_cuenta_estado, na.rm = TRUE) ]
dataset[ , mv_marca_atraso         := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]

dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_tconsumos            := rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ) ]
dataset[ , mv_tadelantosefectivo   := rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

#----------

#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , kclase_nomcampo ) , kclase_nomcampo )
setcolorder( dataset, nuevo_orden )
colnames(dataset)


#grabo el archivo completo
setwd( kcarpeta_datasets )
fwrite( dataset, file=karchivo_salida_completo, sep=kcampos_separador, na="", row.names=FALSE )

#comprimo el archivo con gzip
system(  paste("gzip -f", karchivo_salida_completo) )

#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   unique( dataset[ , get(kcampo_foto) ] ) 

#creo la carpeta donde van los resultados
dir.create(file.path(kcarpeta_datasets, kextension), showWarnings = FALSE)

#grabo todas las fotos
lapply(  fotos_distintas,  fguardar_foto,  pdataset=dataset ) 

t1   <-  Sys.time()

tiempo_corrida <-  as.numeric( t1 - t0, units = "secs")


#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


