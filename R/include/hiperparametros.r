#------------------------------------------------------

hiperparametros_titulos = function( phiper, parchivo )
{
  #escribo los  titulos  del archivo salida
  setwd(phiper$directory)
  if(!file.exists(parchivo))
  {
    cat("experimento",
        "metrica1_actual",
        "metrica2_actual",
        "metrica1_futuro",
        "metrica2_futuro",
        "canaritos_muertos",
        "tiempo",
        "pcorte",
        "parametros",
        "fecha", 
        "clase", "programa", "algoritmo", "busqueda" , "estimacion",
        "dataset_actual_train", "dataset_actual_test", "dataset_futuro_train", "dataset_futuro_test", "observaciones",
        "\n", 
        sep=phiper$separador, 
        file=parchivo, 
        fill=FALSE, 
        append=FALSE)
  }

}
#------------------------------------------------------

hiperparametros_crear = function( phiper)
{
  hiperparametros_titulos( phiper, phiper$arch_global ) 
  hiperparametros_titulos( phiper, phiper$arch_local ) 
}
#------------------------------------------------------
#dibujo la evolucion de la metrica

hiperparametros_refrescar  = function( phiper )
{  
 
  campo_metrica  <- "metrica1_actual"
  campo_metrica2 <- "metrica2_actual"
  campo_metrica_futuro  <- "metrica1_futuro"
  campo_tiempo   <- "tiempo"

  #leo el archivo de salida, que tiene la info para graficar la evolucion
  setwd(phiper$directory)
  salida <-  read.table(phiper$arch_local, header=TRUE, sep=phiper$separador)
  salida <-  subset(salida, experimento== phiper$experimento ) 

  if(nrow(salida) >= 1)
  {

    #una hora tiene 3600 segundos, un dia tiene 24 horas
    tiempoacum  <- cumsum( salida[, (campo_tiempo)  ] ) /(3600*24)
    metricamax  <- cummax( salida[, (campo_metrica)  ])

  
    #dibujo la curva
    setwd(directory$work)
    jpeg(file = phiper$arch_imagen,  width = 10, height = 8, units = 'in', res = 300)

    #dos graficos en una misma salida
    par(mfrow=c(2,1))

    tituvar  <-  paste("(iter=", nrow(salida), " max=", max(salida[ , (campo_metrica) ]),  ")")
    plot(tiempoacum, 
          metricamax, 
          type="n",
          main=paste("Evolucion Metrica", tituvar), 
          xlab="tiempo en DIAS ", 
          ylab="Metrica ", 
          pch=19)

    lines(tiempoacum, metricamax, type="l" , col="red", lwd=2)
    lines(tiempoacum, salida[ , (campo_metrica) ], type="l" , col="blue")

 
    salida  <-  as.data.table( salida )
    salida2 <-  salida[ order(get(campo_metrica), decreasing=TRUE),  ]

    salida3 <-  as.data.table(mutate( salida2, idtemp :=  row_number()))

    x1      <-  salida3[, idtemp ]
    y1      <-  salida3[ , get(campo_metrica_futuro) ]
  
    salida4 <-  salida[ order(get(campo_metrica2), decreasing=TRUE),  ]
    salida5 <-  as.data.table(mutate( salida4, idtemp :=  row_number()))



    x2      <-  salida5[, idtemp ]
    y2      <-  salida5[ , get(campo_metrica_futuro) ]


    tituvar  <-  paste( "azul gan, verde auc", nrow(salida) )
    plot( x1, 
          y1, 
          type="n",
          main=paste("Evolucion Metrica", tituvar), 
          xlab="iteracion", 
          ylab="metrica ", 
          ylim= c( pmin( min(y1),  min(y2) ), 
                   pmax( max(y1),  max(y2) ) ),
          pch=19)

    lines(x1, y1, type="l" , col="blue", lwd=1)
    lines(x2, y2, type="l" , col="green3")

    dev.off()

  }
}
#------------------------------------------------------------------------------


hiperparametros_grabar1 = function( phiper, parchivo,
                                    pactual_ganancia, pactual_auc, pfuturo_ganancia, pfuturo_auc, ptiempo, pprob_corte, pst_parametros, 
                                    parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase, pcanaritos_muertos )
{

  setwd(phiper$directory)

  cat( phiper$experimento,
       pactual_ganancia, 
       pactual_auc,
       pfuturo_ganancia, 
       pfuturo_auc,
       pcanaritos_muertos,
       ptiempo,
       pprob_corte,
       pst_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       ifelse( is.na( pclase), phiper$clase_tipo,  pclase ),
       phiper$programa, 
       phiper$algoritmo, 
       phiper$busqueda, 
       phiper$estimacion,
       parchivo_actual_train, parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test,
       phiper$observaciones,
       "\n", 
      sep=phiper$separador, 
      file=parchivo, 
      fill=FALSE, append=TRUE 
    )

}
#------------------------------------------------------------------------------

hiperparametros_grabar = function( phiper,
                                   pactual_ganancia, pactual_auc, pfuturo_ganancia, pfuturo_auc, ptiempo, pprob_corte, pst_parametros, 
                                   parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase,
                                   pcanaritos_muertos)
{
  #grabo el archivo  GLOBAL
  hiperparametros_grabar1(phiper, phiper$arch_global,
                        pactual_ganancia, pactual_auc, pfuturo_ganancia, pfuturo_auc, ptiempo, pprob_corte, pst_parametros, 
                        parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase, pcanaritos_muertos )

  #grago el archivo  LOCAL
  hiperparametros_grabar1(phiper, phiper$arch_local,
                        pactual_ganancia, pactual_auc, pfuturo_ganancia, pfuturo_auc, ptiempo, pprob_corte, pst_parametros, 
                        parchivo_actual_train,  parchivo_actual_test, parchivo_futuro_train, parchivo_futuro_test, pclase, pcanaritos_muertos )

}
#------------------------------------------------------------------------------
