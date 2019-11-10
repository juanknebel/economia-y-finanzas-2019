
#se trabaja con constantes para ordenar el codigo fuente


#source("M:\\R\\elementary\\canaritos_dibujo.r")

#limpio la memoria
rm(list=ls())
gc()


library("data.table")
library("rpart")
library("rpart.plot")


#Parametros entrada
karchivo_entrada      <-  "M:\\datasets\\201902.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c(kcampo_id)


#Parametros salida
karchivo_imagen1       <-  "M:\\work\\canaritos_010.jpg"
karchivo_imagen2       <-  "M:\\work\\canaritos_011.jpg"



#cargo los datos
dataset <- fread(karchivo_entrada)



#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ]

columnas_original <- colnames( dataset )

magic_canaritos <- 0.1 
canaritos_cantidad <- as.integer( round(ncol(dataset) * magic_canaritos) )
for( i in 1:canaritos_cantidad )
{
  dataset[        , paste0( "canarito", i ) :=  runif( nrow(dataset) ) ]
}

columnas_canaritos <-   colnames( dataset )[ which( colnames( dataset ) %like% "canarito" ) ]

# generacion del modelo
formula  <-  formula(paste(kclase_nomcampo, "~ ."))

t0       <-  Sys.time()
modelo   <-  rpart(formula,   data = dataset,   cp=0.0, maxdepth=8, minsplit=20, minbucket=5,  xval=5)
t1       <-  Sys.time()

tcorrida <-  as.numeric( t1 - t0, units = "secs")
print( tcorrida)

#cuento cuantas variables canarito distintas aparecen
frame  <- modelo$frame
leaves <- frame$var == "<leaf>"
used   <- unique(frame$var[!leaves])
canaritos_muertos <- sum( unlist( used ) %like% "canarito" )
cat( "canaritos_muertos: ", canaritos_muertos )


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen1,  width = 30, height = 6, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, snip=TRUE)

#------------------------------------------------------------------------------
#Ahora hago que los canaritos sean las primeras variables del dataset
#tengo gran curiosidad por ver si sale el mismo arbol

setcolorder(  dataset,  unique( c( columnas_canaritos, columnas_original ) ) )


t0       <-  Sys.time()
modelo   <-  rpart(formula,   data = dataset,   cp=0.0, maxdepth=8, minsplit=20, minbucket=5,  xval=0)
t1       <-  Sys.time()

tcorrida <-  as.numeric( t1 - t0, units = "secs")
print( tcorrida)

#cuento cuantas variables canarito distintas aparecen
frame  <- modelo$frame
leaves <- frame$var == "<leaf>"
used   <- unique(frame$var[!leaves])
canaritos_muertos <- sum( unlist( used ) %like% "canarito" )
cat( "canaritos_muertos: ", canaritos_muertos )

#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen2,  width = 30, height = 6, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

