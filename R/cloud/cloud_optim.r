#source("~/cloud/cloud1/R/inicial/cloud_optim.r")


#limpio la memoria
rm(list=ls())
gc()



library("data.table")



directory.datasets.bucket  <-  "~/cloud/cloud1/datasets/"
directory.datasets.local   <-  "~/datasets/"

karchivo_entrada           <-  "paquete_premium.txt"
karchivo_entrada_zip       <-  "paquete_premium.zip"
kcampos_separador          <-  "\t"


setwd(directory.datasets.bucket)
t0       <-  Sys.time()
dataset  <-  fread(cmd=paste("cat", karchivo_entrada), header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_cat_bucket <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde el bucket con cat ",  tcorrida_cat_bucket, "\n")


#borro el dataset y libero memoria
rm(dataset)
gc()


setwd(directory.datasets.bucket)
t0       <-  Sys.time()
dataset  <-  fread(cmd=paste("gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_bucket_zip <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde el bucket con cat ",  tcorrida_bucket_zip, "\n")


#borro el dataset y libero memoria
rm(dataset)
gc()



/*----------------------*/



#creo la carpeta local
dir.create("~/datasets/")


#copio del bucket al disco local
setwd(directory.datasets.bucket )
t0       <-  Sys.time()
file.copy( c(karchivo_entrada),   directory.datasets.local )
t1       <-  Sys.time()
tcorrida_cp <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo copiado  bucket -> local ",  tcorrida_cp, "\n")


#copio del bucket al disco local
setwd(directory.datasets.bucket )
t0       <-  Sys.time()
file.copy( c(karchivo_entrada_zip),   directory.datasets.local )
t1       <-  Sys.time()
tcorrida_cp_zip <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo copiado  bucket -> local ",  tcorrida_cp_zip, "\n")



#mido tiempo carga tabla desde maquina local
setwd(directory.datasets.local)
t0       <-  Sys.time()
dataset  <- fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_local <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde maquina local ",  tcorrida_local, "\n")



#borro el dataset y libero memoria
rm(dataset)
gc()



setwd(directory.datasets.local)
t0       <-  Sys.time()
dataset  <-  fread(cmd=paste("cat", karchivo_entrada), header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_cat_local <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde el bucket con cat ",  tcorrida_cat_local, "\n")


#borro el dataset y libero memoria
rm(dataset)
gc()




setwd(directory.datasets.local)
t0       <-  Sys.time()
dataset  <-  fread(cmd=paste("gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_local_zip <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde el bucket con cat ",  tcorrida_local_zip, "\n")



#borro el dataset y libero memoria
rm(dataset)
gc()



setwd(directory.datasets.bucket)
t0       <-  Sys.time()
dataset  <-  fread(karchivo_entrada, header=TRUE, sep=kcampos_separador)
t1       <-  Sys.time()
tcorrida_bucket <-  as.numeric( t1 - t0, units = "secs")
cat("tiempo carga dataset desde el bucket ",  tcorrida_bucket, "\n")



#borro el dataset y libero memoria
rm(dataset)
gc()


#limpio la memoria
rm(list=ls())
gc()


