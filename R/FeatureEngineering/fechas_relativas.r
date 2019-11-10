#Objetivo : pasar los campos de fechas absolutas a fechas relativas

#paso los campos fecha a la cantidad de dias de la foto


#source("~/cloud/cloud1/R/FeatureEngineering/fechas_relativas.r")


#limpio la memoria
rm(list=ls())
gc()



library("data.table")


kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"

kcampos_separador        <-  "\t"
karchivo_entrada_zip     <-  "paquete_premium.zip"

kcampo_foto              <- "foto_mes"

karchivo_salida_prefijo  <-  "./dias/"
karchivo_salida_sufijo   <-  "_dias.txt"


#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  = function(pfoto_mes, pfecha)
{
  
  foto_mes       <- as.POSIXlt(as.Date( paste(pfoto_mes, "01", sep=""), format='%Y%m%d' ))
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date( as.character(pfecha), format='%Y%m%d' )

  return(as.numeric(difftime(foto_mes, fecha, units = c("days"))))
}
#------------------------------------------------------
#guarda el archivo de un mes

fguardar_foto  = function(pfoto_mes, pdataset)
{
  
  dataset_mes   <-   pdataset %>% filter(get(kcampo_foto) == pfoto_mes)

  archivo_salida_mes <- paste(karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="")

  fwrite( dataset_mes, file=archivo_salida_mes , sep=kcampos_separador, na="", row.names=FALSE) 
}
#------------------------------------------------------



setwd(kcarpeta_datasetsOri)

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread( cmd=paste("gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador) 


nrow(dataset)
ncol(dataset)


#paso los campos fecha a dias relativos

dataset[ , Master_Fvencimiento    := fdias_entre(get(kcampo_foto), Master_Fvencimiento) ]
dataset[ , Master_Finiciomora     := fdias_entre(get(kcampo_foto), Master_Finiciomora) ]
dataset[ , Master_fultimo_cierre  := fdias_entre(get(kcampo_foto), Master_fultimo_cierre) ]
dataset[ , Master_fechaalta       := fdias_entre(get(kcampo_foto), Master_fechaalta) ]

dataset[ , Visa_Fvencimiento    := fdias_entre(get(kcampo_foto), Visa_Fvencimiento) ]
dataset[ , Visa_Finiciomora     := fdias_entre(get(kcampo_foto), Visa_Finiciomora) ]
dataset[ , Visa_fultimo_cierre  := fdias_entre(get(kcampo_foto), Visa_fultimo_cierre) ]
dataset[ , Visa_fechaalta       := fdias_entre(get(kcampo_foto), Visa_fechaalta) ]



#ordeno el dataset
setorder(  dataset,  foto_mes, numero_de_cliente )


#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   unique( dataset[ , get(kcampo_foto)] )

#grabo los archivos de cada mes
setwd(kcarpeta_datasets)
lapply( fotos_distintas,  fguardar_foto,  pdataset=dataset) 


#grabo el archivo completo
fwrite( dataset,  
        file="~/cloud/cloud1/datasets/paquete_premium_dias.txt", 
        sep=kcampos_separador, na="", row.names=FALSE) 

#grabo solo el ultimo ano
fwrite( dataset[ foto_mes>=201805 & foto_mes<=201904,  ],
        file="~/cloud/cloud1/datasets/paquete_premium_dias_1ano.txt", 
        sep=kcampos_separador, na="", row.names=FALSE) 



#limpio la memoria

rm(list=ls())
gc()



quit(save="no")


