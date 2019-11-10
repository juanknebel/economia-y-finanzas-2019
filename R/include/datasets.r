#------------------------------------------------------------------------------

dataset_cargar_directo = function( pdata )
{
  setwd( env$directory$datasets)

  varchivos  <- unlist(strsplit( pdata$archivo_actual_train, split=","))
  dataset_actual_train  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador))) 
  #borro las variables que no me interesan
  dataset_actual_train[,  (pdata$campos_a_borrar) := NULL    ] 

  varchivos  <- unlist(strsplit( pdata$archivo_actual_test, split=","))
  dataset_actual_test  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador))) 
  #borro las variables que no me interesan
  dataset_actual_test[,  (pdata$campos_a_borrar) := NULL    ] 


  varchivos  <- unlist(strsplit( pdata$archivo_futuro_train, split=","))
  dataset_futuro_train  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador))) 
  #borro las variables que no me interesan
  dataset_futuro_train[,  (pdata$campos_a_borrar) := NULL    ] 

  varchivos  <- unlist(strsplit( pdata$archivo_futuro_test, split=","))
  dataset_futuro_test  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador))) 
  #borro las variables que no me interesan
  dataset_futuro_test[,  (pdata$campos_a_borrar) := NULL    ] 


  return( list(  
                 "actual_train" = dataset_actual_train,
                 "actual_test"  = dataset_actual_test,
                 "futuro_train" = dataset_futuro_train,
                 "futuro_test"  = dataset_futuro_test
              )
        )
}
#------------------------------------------------------------------------------

dataset_cargar_montecarlo = function( pdata )
{
  #cargo los archivos de entrada
  setwd( env$directory$datasets)
  varchivos  <- unlist(strsplit( pdata$archivo_actual_train, split=","))
  dataset_actual  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador)))
  
  #borro las variables que no me interesan
  dataset_actual[,  (pdata$campos_a_borrar) := NULL    ] 


  setwd( env$directory$datasets)
  varchivos  <- unlist(strsplit(pdata$archivo_futuro_test, split=","))
  dataset_futuro_test  <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador)))
  
  #borro las variables que no me interesan
  dataset_futuro_test[,  (pdata$campos_a_borrar) := NULL    ] 


  return( list(  
                 "actual"       = dataset_actual,
                 "futuro_train" = dataset_actual,
                 "futuro_test"  = dataset_futuro_test
              )
        )
}
#------------------------------------------------------------------------------

dataset_cargar_meses = function(pdata, pstringsAsFactors=FALSE, pimputar_nulos=FALSE )
{
  #cargo los archivos de entrada
  setwd( env$directory$datasets)
  varchivos  <- unlist(strsplit(pdata$archivo_entrada, split=","))
  dataset    <- do.call(rbind, lapply(varchivos, function(x) fread(cmd=paste("cat", x), header=TRUE, sep=pdata$campos_separador, stringsAsFactors=pstringsAsFactors )))
  
  #borro las variables que no me interesan
  dataset[,  (pdata$campos_a_borrar) := NULL    ] 

  #imputo los nulos, ya que ranger no acepta nulos
  if( pimputar_nulos )
  {
    dataset <-  na.roughfix( dataset )
  }

  dataset_actual_test  <<- as.data.table( dataset %>% filter(get(pdata$campo_foto) == pdata$ldata$mes_actual_test) )
  dataset_futuro_test  <<- as.data.table( dataset %>% filter(get(pdata$campo_foto) == pdata$ldata$mes_futuro_test) )

  return( list(  "completo"   = dataset,
                 "actual_test"= dataset_actual_test,
                 "futuro_test"= dataset_futuro_test
              )
        )
}
#------------------------------------------------------------------------------

dataset_generar_meses = function( pdataset, pdata,  pmeses_cantidad, psample=1.0 )
{
  fotos_distintas <-   sort( pdataset %>% distinct(get(pdata$campo_foto)) %>% pull() )

  hasta <- match(  pdata$ldata$mes_actual_test,  fotos_distintas )[1] - pdata$ldata$mes_step
  desde <- hasta - pmeses_cantidad + 1 
  sub   <- fotos_distintas[ desde:hasta ] 
  dataset_actual_train  <- as.data.table( pdataset %>% filter(get(pdata$campo_foto) %in%  sub ) )
  dataset_actual_st     <- paste( lapply( sub, paste, pdata$ldata$mes_sufijo, sep="" ), collapse=',' )
  if( psample < 1.0 )
  {
    set.seed( 33 )
    dataset_actual_train <- as.data.table( dataset_actual_train %>%
  group_by(!!as.name(pdata$clase_nomcampo )) %>%
  sample_frac(psample) %>%
  ungroup)
  }


  hasta <- match(  pdata$ldata$mes_futuro_test,  fotos_distintas )[1] - pdata$ldata$mes_step
  desde <- hasta - pmeses_cantidad + 1 
  sub   <-  fotos_distintas[ desde:hasta ] 
  dataset_futuro_train  <- as.data.table( pdataset %>% filter(get(pdata$campo_foto) %in%  sub ) )
  if( psample < 1.0 )
  {
    set.seed( 33 )
    dataset_futuro_train <- as.data.table( dataset_futuro_train %>%
  group_by(!!as.name(pdata$clase_nomcampo )) %>%
  sample_frac(psample) %>%
  ungroup)
  }

  dataset_futuro_st     <- paste( lapply( sub, paste, pdata$ldata$mes_sufijo, sep="" ), collapse=',' )

  return( list( "actual_train"     = dataset_actual_train,
                "futuro_train"     = dataset_futuro_train,
                "actual_train_st"  = dataset_actual_st,
                "futuro_train_st"  = dataset_futuro_st
              )
        )
}

#------------------------------------------------------------------------------
