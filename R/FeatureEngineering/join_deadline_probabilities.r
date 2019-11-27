library(data.table)
library(tidyverse)
library(janitor)

## ----------- Preprocesamiento en caso de ser necesario ----------- ##
# Solo en caso de que los archivos esten separados antes deben ser unidos
setwd("~/cloud/cloud1/work/1004/")
#setwd("~/git/economia-y-finanzas-2019/datasets/")

lm_original1 <- fread("lightgbm_dead_line_1004_201806_probabilidades.txt")
lm_original2 <- fread("lightgbm_dead_line_1004_201807_probabilidades.txt")
lm_original3 <- fread("lightgbm_dead_line_1004_201808_probabilidades.txt")
lm_original4 <- fread("lightgbm_dead_line_1004_201809_probabilidades.txt")
lm_original5 <- fread("lightgbm_dead_line_1004_201810_probabilidades.txt")
lm_original6 <- fread("lightgbm_dead_line_1004_201811_probabilidades.txt")
lm_original7 <- fread("lightgbm_dead_line_1004_201812_probabilidades.txt")
lm_original8 <- fread("lightgbm_dead_line_1004_201901_probabilidades.txt")
lm_original9 <- fread("lightgbm_dead_line_1004_201902_probabilidades.txt")
lm_original10 <- fread("lightgbm_dead_line_1004_201903_probabilidades.txt")
lm_original11 <- fread("lightgbm_dead_line_1004_201904_probabilidades.txt")

# Agrego el mes en caso de que no lo tenga
lm_original1[, (c("foto_mes")) := 201806]
lm_original2[, (c("foto_mes")) := 201807]
lm_original3[, (c("foto_mes")) := 201808]
lm_original4[, (c("foto_mes")) := 201809]
lm_original5[, (c("foto_mes")) := 201810]
lm_original6[, (c("foto_mes")) := 201811]
lm_original7[, (c("foto_mes")) := 201812]
lm_original8[, (c("foto_mes")) := 201901]
lm_original9[, (c("foto_mes")) := 201902]
lm_original10[, (c("foto_mes")) := 201903]
lm_original11[, (c("foto_mes")) := 201904]

# Uno todas las probabilidades
lm_final = rbind(lm_original1, lm_original2, lm_original3, lm_original4,
                 lm_original5, lm_original6, lm_original7, lm_original8,
                 lm_original9, lm_original10, lm_original11)

fwrite(lm_final,
       file="lightgbm_dead_line_1004_probabilidades.txt",
       sep="\t")

## ----------- Aca comienza el join de atributos ----------- ##
setwd("~/cloud/cloud1/datasets/")
lm_original <- fread("lightgbm_dead_line_1004_all_probabilities")
exthist <- fread("paquete_premium_exthist.txt.gz")

#setwd("~/git/economia-y-finanzas-2019/datasets/")
#lm_original <- fread("lightgbm_dead_line_1004_all_probabilities")
#exthist <- fread("paquete_reducido2.csv")

dim(lm_original)

# Distribucion x mes
dist_orig <- exthist$foto_mes %>% 
  janitor::tabyl() %>% 
  janitor::adorn_pct_formatting()

names(dist_orig)[1] <- "foto_mes" 

# Para ver si tengo la misma cantidad de registros
dist_mod <- dist_orig %>% 
  dplyr::filter(foto_mes >= 201706)
sum(dist_mod$n)

## Asigno nombre a la prob_lmuerte
# OJO esto puedo cambiar de acuerdo a como se haya grabado el archivo
names(lm_original)[3] <- "prob_lmuerte_orig" 

## Left Join por "numero_de_cliente" "foto_mes"
exthist_lm <- left_join(exthist, lm_original, 
                        by = c("numero_de_cliente" = "numero_de_cliente", 
                               "foto_mes" = "foto_mes"))

## Cross Tab para validación de las clases
ctab_table<- table(exthist_lm$clase_ternaria, exthist_lm$clase01) #define object w/table parameters for simple calling
ftable(ctab_table)

## Elimino Variable clase01 para no afectar la predicción
exthist_lm <-  exthist_lm[ ,!(names(exthist_lm) == "clase01")]


## Grabo Archivo con prob de Linea de Muerte Original
setwd("~/cloud/cloud1/datasets/")
#setwd("~/git/economia-y-finanzas-2019/datasets/")
fwrite(exthist_lm,
       file="paquete_premium_exthist_lm_original.txt.gz",
       sep="\t")











