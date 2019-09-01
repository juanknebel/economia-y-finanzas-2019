#Objetivo:  mostrar ganancia de predicados



library("data.table")


#cargo los datos
dataset <- fread("M:\\datasets\\201902.txt")

#El universo agrupado por  clase_ternaria
ftable(dataset$clase_ternaria)


#calculo la ganancia
dataset[  , ganancia:= -500 ]
dataset[ clase_ternaria=='BAJA+2' ,   ganancia:= 19500]
sum( dataset$ganancia )


#clase
dataset[  , clase:= 0 ]
dataset[ clase_ternaria=='BAJA+2' ,   clase:= 1]
sum( dataset$clase )


#Otra forma de ver el universo
ftable(dataset[  ,clase_ternaria])

#Basico
summary( dataset)

by(dataset, dataset$clase_ternaria, summary)


#Corte por edad
hist(dataset[ ,cliente_edad] )
boxplot(cliente_edad  ~ clase_ternaria,  data=dataset)

ftable(dataset[ cliente_edad <=33, clase_ternaria])
ftable(dataset[ cliente_edad  >33, clase_ternaria])

sum( dataset[ cliente_edad <=33, ganancia] )


(sum( dataset[ cliente_edad <=33, clase]) / nrow( dataset[ cliente_edad <=33, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))

	
#Corte por mcuentas_saldo
hist(dataset[ ,mcuentas_saldo] )
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset)
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset, outline=FALSE)

ftable(dataset[ mcuentas_saldo <= -120000, clase_ternaria])
ftable(dataset[ mcuentas_saldo >  -120000, clase_ternaria])




#Corte por Visa_mconsumototal
hist(dataset[ ,Visa_mconsumototal] )
boxplot(Visa_mconsumototal  ~ clase_ternaria, data=dataset, outline=FALSE)

ftable(dataset[ Visa_mconsumototal <= 20000, clase_ternaria])
ftable(dataset[ Visa_mconsumototal >  20000, clase_ternaria])
ftable(dataset[ is.na(Visa_mconsumototal)  , clase_ternaria])



#Tabla de contingencia , ya que  ttarjeta_visa  es una variable DISCRETA
table( dataset$ttarjeta_visa,  dataset$clase_ternaria, useNA = 'ifany')

dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = ttarjeta_visa]



#Tabla de contingencia , ya que  Visa_cuenta_estado  es una variable DISCRETA
table( dataset$Visa_cuenta_estado,  dataset$clase_ternaria, useNA = 'ifany')

dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = Visa_cuenta_estado]



#Tabla de contingencia , ya que  Master_cuenta_estado  es una variable DISCRETA
table( dataset$Master_cuenta_estado,  dataset$clase_ternaria, useNA = 'ifany')


dataset[, list( gan=sum(ganancia), lift= (sum(clase)/(.N))/(1085/187861) ), by = Master_cuenta_estado]




#algo un poco mas avanzado 

library(caret)

d2 <- as.data.table(downSample( dataset, as.factor(dataset$clase_ternaria), list = FALSE, yname = "clase_ternaria2"))

d3 <-  d2[Visa_mconsumototal < 20000 & mcuentas_saldo < 20000, ]

my_cols <- c("#00FF00", "#FF0000", "#0000FF")

pairs(d3[ , c("mcuentas_saldo", "Visa_mconsumototal") ], pch = 19,  cex = 0.5,
       col = my_cols[ as.factor(d3$clase_ternaria) ],
      lower.panel=NULL)

