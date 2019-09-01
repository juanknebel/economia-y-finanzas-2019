#Arbol con libreria  rpart
#se trabaja con constantes para ordenar el codigo fuente


#source("M:\\R\\elementary\\MiPrimerArbol_01.r")

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
karchivo_imagen       <-  "M:\\work\\arbol_01.jpg"



#cargo los datos
dataset <- fread(karchivo_entrada)


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ]


# generacion del modelo
formula  <-  formula(paste(kclase_nomcampo, "~ ."))

t0       <-  Sys.time()
modelo   <-  rpart(formula,   data = dataset,   cp=0.01,   xval=0)
t1       <-  Sys.time()

tcorrida <-  as.numeric( t1 - t0, units = "secs")
print( tcorrida)


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen,  width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

