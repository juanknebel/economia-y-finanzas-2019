#Objetivo:  generar un arbol CREANDO una nueva variable que es la combinacion de las dos mas importantes

#Arbol con libreria  rpart
#se trabaja con constantes para ordenar el codigo fuente


#source("M:\\R\\elementary\\MiPrimerArbol_02.r")

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
karchivo_imagen       <-  "M:\\work\\arbol_02.jpg"



#cargo los datos
dataset <- fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)


#Agrego la nueva variable  MAX(Visa_cuenta_estado, Master_cuenta_estado) 
dataset[ ,  VisaMaster_cuenta_estado1 :=  pmax(Visa_cuenta_estado, Master_cuenta_estado, na.rm = TRUE)  ]


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

