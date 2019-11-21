#Pruning RECURSIVO
#Randomizacion de la clase
#100-permutaciones  muuuuy lento
#la pregunta es como se relacionan los canaritos con este pruning
#deberia cortar por encima de los canaritos ?
#En construccion


#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")
library("parallel")

kminbucket <-  5
kminsplit  <-  5
karchivo_arbol <- "arboljueves.jpg"


kcarpeta_datasets  <- "~/cloud/cloud1/datasets/dias/"
kcarpeta_work      <- "~/cloud/cloud1/work/"

kcarpeta_datasets  <- "~/azure/noborrar/"
kcarpeta_work      <- "~/azure/noborrar/"
kcores             <-  60

karchivo_salida    <-  "sincanaritos_39.txt"
karchivo_detalle   <-  "sincanaritos_39_detail.txt"

kprob_corte <- 0.8151659
kprob_corte <- 0.025
kprob_corte <- 0.5
koversampling  <-  39
kmaxdepth      <-  6

#------------------------------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +19500 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -500 )

fmetrica_ganancia_rpart  = function( probs, clases, pclase_valor_positivo, problema )
{
 
  return(  sum(    (probs > kprob_corte) * 
                   ifelse( clases== pclase_valor_positivo, 19500, -500 )   
              )
         )
}
#------------------------------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases, pclase_valor_positivo )
{
  testing_binaria  <-  as.numeric( clases == pclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc") 
 
  return( unlist(auc_testing@y.values) )

}

#------------------------------------------------------------------------------

agregar_canaritos <- function( pdataset, pcanaritos_idx )
{

  canaritos_cantidad <- as.integer( round(ncol(pdataset) * pcanaritos_idx) )
  vcanaritos <-  paste0( "canarito", 1:canaritos_cantidad )

  #uso esta semilla para los canaritos
  set.seed(10217)

  #podria haber hecho un loop for
  pdataset[ , (vcanaritos) := 0 ]
  pdataset[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]

  #ahora hago que los canaritos sean las primeras variables del dataset
  nuevo_orden <-  c( vcanaritos, setdiff( colnames( pdataset), vcanaritos )) 
  setcolorder( pdataset, nuevo_orden )
}
#------------------------------------------------------------------------------

split_randomize  <- function( pdummy, pdataset )
{
  clase_nueva <- sample( pdataset$clase_ternaria, replace=FALSE )
  pdataset[ , clase_ternaria:= clase_nueva ]
  
  pesos <-  ifelse(  pdataset$clase_ternaria=="BAJA+2", koversampling, 1.0 )
  
  m <- do.call( rpart, list( formula="clase_ternaria ~ . ",
               data = pdataset, 
               weights = pesos,
               minbucket=kminbucket, minsplit=kminsplit, cp=0,   xval=0,  maxdepth= kmaxdepth)
              )

  tt <- m$frame[1,c("var","complexity")]

  return( tt )
}

#------------------------------------------------------------------------------

prunear <-  function( pid, pumbral, parchivo, precursive )
{
  cat(  "pid:", pid , " " ) 
  
  if( estructura[id==pid, var ] == "<leaf>" )  return() 

  if( nrow(estructura[ id==pid & is.na(pvalue),  ])  > 0 )
  {
      buenos <- unique( tb_clausura[ padre==pid, id ] )

      dtrain <-  copy( dataset_train[ modelo$where %in% buenos , ]  )
      
      if(  nrow(dtrain) !=  estructura[id==pid, n ] )
      {
        cat( "Error Fatal",  nrow(dtrain), 1, "\n")
      }
      
      
      cat( nrow( dtrain[clase_ternaria=="BAJA+2", ] ), nrow( dtrain[clase_ternaria!="BAJA+2", ] ) )
      vpesos <-  ifelse(  dtrain$clase_ternaria=="BAJA+2", koversampling, 1.0 )
     
      m2  <-  do.call( rpart, list( formula="clase_ternaria ~ ." , data = dtrain,  
                              weights = vpesos,
                              minbucket=kminbucket, minsplit=kminsplit, cp=0,   xval=0,  maxdepth=kmaxdepth) 
                     )

      tbl <- m2$frame[1,c("var","complexity")]

      tr <- as.data.table( tbl  )
      tr[ , pos:= -1 ]
      dtrain_randomize <-  copy( dtrain )

      regs <- nrow(dtrain)
      tope <- 10000
     # if( regs < 10000 )  tope <-  300
     # if( regs <  5000 )  tope <-  500
     # if( regs <  1000 )  tope <- 1000  
     # if( regs <   500 )  tope <- 1500
     # if( regs <   100 )  tope <- 2000

      res <- split_randomize( 0, dtrain_randomize )
      
      res  <-  mclapply( 1:tope, split_randomize, dtrain_randomize, mc.preschedule=TRUE, mc.set.seed=TRUE, mc.cores= kcores )
      res2 <-  rbindlist( res)
      res2[ , pos:= 0 ]
      tr <- rbind( tr, res2 )
      tr[  , azar := runif(nrow(tr) )]
      
      setorderv( tr, c("complexity", "azar", "pos"), c(-1,-1,-1) )
        
      if( nrow(tr[ pos==0 & var=="<leaf>", ])  == nrow(tr[ pos==0, ]) )
      {
        p_value <- 0
      } else { 
           p_value <-   which( tr[ var!="<leaf>" ,  pos==-1 ] ) / nrow( tr[ var!="<leaf>"] )
      }
      
      tr[ , azar:= NULL ]
      tr[ , id:= pid ]

      tb_comparaciones <<- rbind( tb_comparaciones, tr )
      
      cat( "[ ",  nrow(tr[ pos==0 & var=="<leaf>", ]) , nrow(tr[ pos==0, ]),  p_value, "]\n" )
      estructura[ id==pid,  pvalue := p_value ]
      estructura[ id==pid,  experimentos := nrow(tr[ var!="<leaf>"])   - 1 ]
      estructura[ id==pid,  var2 := tbl$var ]
      estructura[ id==pid,  pos  := nrow( dtrain[clase_ternaria=="BAJA+2", ] )]
      estructura[ id==pid,  neg  := nrow( dtrain[clase_ternaria!="BAJA+2", ] )]
      
      
      setwd(  kcarpeta_work )
      fwrite( estructura,  file=parchivo, sep="\t" )
      fwrite( tb_comparaciones,  file=karchivo_detalle, sep="\t" )
      
      
      rm( dtrain_randomize )
      rm( dtrain )
      rm( tr )
      gc()
  }

  p_value <-   estructura[ id==pid,  pvalue]
  #recursividad
  if( precursive & (p_value < pumbral) )
  {
     prunear( estructura[ id==pid, hijo1 ], pumbral, parchivo, precursive )
     prunear( estructura[ id==pid, hijo2 ], pumbral, parchivo, precursive  )
  }

}
#------------------------------------------------------------------------------

verificar <-  function( pid )
{
  cat(  "pid:", pid , "\n" ) 
  
  if( estructura[id==pid, var ] == "<leaf>" )  return() 
  
  buenos <- unique( tb_clausura[ padre==pid, id ] )

  dtrain <-  copy( dataset_train[ modelo$where %in% buenos , ]  )
  
  if(  nrow(dtrain) !=  estructura[id==pid, n ] )
  {
    cat( "Error Fatal",  pid, nrow(dtrain), estructura[id==pid, n ], "\n")
  }

  verificar( estructura[ id==pid, hijo1 ] )
  verificar( estructura[ id==pid, hijo2 ] )

}
#------------------------------------------------------------------------------

#cargo los datasets
setwd( kcarpeta_datasets )
dataset_train <- fread( "201902_dias.txt" )
dataset_test  <- fread( "201904_dias.txt" )

dataset_train[  , clase_ternaria:= ifelse( clase_ternaria=="BAJA+2", "BAJA+2", "CONTINUA" ) ]
dataset_test[   , clase_ternaria:= ifelse( clase_ternaria=="BAJA+2", "BAJA+2", "CONTINUA" ) ]

#dataset_train[  , clase_ternaria:= ifelse( clase_ternaria=="BAJA+2", "SI", "NO" )]

#agregar_canaritos( dataset_train, 0.4 )
#agregar_canaritos( dataset_test,  0.4 )

vpesos <-  ifelse(  dataset_train$clase_ternaria=="BAJA+2",  koversampling, 1.0 )

modelo <- rpart( "clase_ternaria ~ ." , data = dataset_train, 
                  weights=vpesos,
                  minbucket=kminbucket, minsplit=kminsplit,  cp=0.0,   xval=0,  maxdepth=20)



#impresion un poco mas elaborada del arbol
setwd(  kcarpeta_work )
jpeg(file = karchivo_arbol,  width = 40, height = 6, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

#clase_nueva <- sample( dataset_train$clase_ternaria, replace=FALSE )
#dataset_train[ , clase_ternaria:= clase_nueva ]
#modelo2 <- rpart( "clase_ternaria ~ ." , data = dataset_train,
#                  minbucket=1,   cp=0.0,   xval=0,  maxdepth=6)


#impresion un poco mas elaborada del arbol
#setwd(  "~/cloud/cloud1/work/")
#jpeg(file = "canaritos_al20.jpg",  width = 100, height = 12, units = 'in', res = 300)
#prp(modelo2, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#dev.off()

if( file.exists(karchivo_salida) )
{
  estructura       <- fread( karchivo_salida )
  tb_comparaciones <- fread( karchivo_detalle )
  
} else {
  estructura <-  modelo$frame[ , c("var", "n","complexity" ) ]
  estructura <-  as.data.table( estructura )
  estructura[ , id := .I ]
  estructura[  , padre:=0 ]

  e <-  copy(estructura)
  e[ , hoja:= 0 ]
  e[ var=="<leaf>",  hoja:=1 ]


  for( nivel  in  (1:30 ) )
  {
    for( i in (1:(nrow(e)-2)))
    {
      if( !e[ i, hoja]  & e[ i+1, hoja]  & e[ i+2, hoja] )
      {
        father <- e[i,id]
        e[ i+1, padre:=  father  ] 
        e[ i+2, padre:=  father  ] 
    
        e[i, hoja:=1 ]
        estructura[  id==e[i+1,id], padre:=father ]
        estructura[  id==e[i+2,id], padre:=father ]
      }
    }
    e <-  e[ padre==0, ]   
    setorder( e, id )
  
  }

  estructura[  , hijo1 := min( estructura[ padre==id, id ] ) ]
  fmin <- function( pid ) {  return( min(estructura[ padre==pid, id ]))}
  fmax <- function( pid ) {  return( max(estructura[ padre==pid, id ]))}

  for( i in min( estructura$id) : max( estructura$id ) )
  {
    estructura[ id==i, hijo1 := fmin( id ) ]
    estructura[ id==i, hijo2 := fmax( id ) ]
  }
  
  estructura[ , visitado:= 0 ]
  estructura[ , calculado:= 0 ]

  tb_comparaciones <- as.data.table( list( "id"=-1, "complexity"=0.0, "pos"=-1, "var"="raiz" ) )

}


tb_clausura <-  unique( estructura[ , c("id","padre"), with=FALSE])

for( i in 1:30 )
{
  tb_clausura <- unique( rbind( tb_clausura, tb_clausura[ tb_clausura, on=list( "id"=padre), c("padre","id"), allow.cartesian=TRUE]))
}

tb_clausura <-  tb_clausura[ !is.na(padre)  & padre != 0 , ]





#llamado recursivo
#horas y horas en correr

t0   <-  Sys.time()
prunear( 1, 0.99, karchivo_salida, precursive=TRUE)
t1   <-  Sys.time()
tiempo_corrida <-  as.numeric( t1 - t0, units = "mins")
tiempo_corrida

#-------------------------------
#-------------------------------
#-------------------------------
#-------------------------------
#-------------------------------
#-------------------------------


arbolito <-  function( pumbral )
{
  modelo_original <- modelo

  #metricas del modelo original
  testing_prediccion_original  <- predict( modelo_original, dataset_test, type = "prob")
  gan_original  <-  fmetrica_ganancia_rpart(testing_prediccion_original[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2")
  auc_original  <-  fmetrica_auc_rpart(testing_prediccion_original[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2" )


  total <-  nrow(  modelo_original$frame )
  for( i in 1:total )
  {
    if( !is.na(estructura[ i, pvalue]) )
    {
    if(  estructura[ i, pvalue] > pumbral )  modelo_original$frame[ i, "complexity"] <- -666
    }
  }

  #pruning propiamente dicho
  modelo_pruned <- prune(  modelo_original, -666.0 )

  #metricas del modelo pruned
  testing_prediccion_pruned  <- predict( modelo_pruned, dataset_test, type = "prob")
  gan_pruned  <-  fmetrica_ganancia_rpart(testing_prediccion_pruned[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2")
  auc_pruned  <-  fmetrica_auc_rpart(testing_prediccion_pruned[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2" )
  
  setwd(  kcarpeta_work )
  varchivo_arbol = paste0( "arbol_pruned_", as.integer(pumbral*100), ".jpg" ) 
  jpeg(file = varchivo_arbol,  width = 40, height = 6, units = 'in', res = 300)
  prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
  dev.off()

  return(  list( "ganancia"=gan_pruned, "auc"=auc_pruned ) )
}
#-------------------------------

#Primero  MENOS del 1% ,para eso hice la brutalidad de diez mil randomizaciones
for( i in 1:10) 
{
   res <- arbolito(  i/1000 )
   cat( i/1000, res$ganancia, res$auc, "\n" )
}

#Ahora del 1% al 100%
for( i in 1:100) 
{
   res <- arbolito(  i/100 )
   cat( i/100, res$ganancia, res$auc, "\n" )
}

res <- arbolito( 1.1 )
print( res )


#--------------------

modelo_original <- modelo

#metricas del modelo original
testing_prediccion_original  <- predict( modelo_original, dataset_test, type = "prob")
gan_original  <-  fmetrica_ganancia_rpart(testing_prediccion_original[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2")
auc_original  <-  fmetrica_auc_rpart(testing_prediccion_original[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2" )


total <-  nrow(  modelo_original$frame )
for( i in 1:total )
{
    if(  estructura[ i, var] %like% "canarito" )  modelo_original$frame[ i, "complexity"] <- -666
}

#pruning propiamente dicho
modelo_pruned <- prune(  modelo_original, -666.0 )

#metricas del modelo pruned
testing_prediccion_pruned  <- predict( modelo_pruned, dataset_test, type = "prob")
gan_pruned  <-  fmetrica_ganancia_rpart(testing_prediccion_pruned[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2")
auc_pruned  <-  fmetrica_auc_rpart(testing_prediccion_pruned[, "BAJA+2" ],  dataset_test[, clase_ternaria], "BAJA+2" )
gan_pruned
auc_pruned
