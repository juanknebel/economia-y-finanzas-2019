## Problema de Attrition

## Problema de Churn
Retención de clientes
De que manera le sirve al banco: 2 meses de antelación

Gradient boosting como XGBoost con optimización bayesiana

la ganancia correctamente calculada con 5-fold montecarlo estimation varia mucho segun los hiperparametros

no usar busqueda grid, utilizar busqueda bayesiana, utilizar feature engineering

optimizacion bayesiana: calcula el maximo de una funcion de n parametros que es costosa computacionalmente

**Poder predictivo en orden creciente**
decision tree <<<<<
random forest <<<
light gbm <
xgboost

| Dataset engineering  | Feature engineering                   | Class engineering                                            |
| -------------------- | ------------------------------------- | ------------------------------------------------------------ |
| 201902 <br /> 201901 | vars mismo mes <br /> vars históricas | CLASE_BINARIA2 <br /> POS {BAJA+2, BAJA+1} <br /> NEG = {CONTINUA} |


```
-----------------------------------------------------------------> MESES
+-----------+    +------+
|   TRAIN   |    | TEST |
+-----------+    +------+
            +-----------+    +------+
            |   TRAIN   |    | TEST |
            +-----------+    +------+
                        +-----------+    +------+
                        |   TRAIN   |    | TEST |
                        +-----------+    +------+
```

## Pequeñas batallas
* undersampling de los continua o e todo el dataset.
* calibración de las probabilidades.

**La curva ROC no cambia al hacer undersampling!!!!**
**Leer el paper de 40 paginas connen**


Aumenté de procesamiento x 10
* Sampling usual.
* Undersampling si hay una clase mayoritaria.

| Todos los datos       | Undersampling 10% de los Neg |
| --------------------- | ---------------------------- |
| Pos 17 <br /> Neg 728 | Pos 17 <br /> Neg 73         |
| Gan(M1) < Gan(M2)     | Gan(M1) < Gan(M2)            |

```
+-----+                   +-----------+                                              +------+
|     | -- X Sampling --> |Small Train| -- Algoritmo --> Mejores hiperparametros --> |Modelo|
|     |                   +-----------+                            ^                 |final |
|     |                           +----------------------+         |                 +------+
|Train|                           |Optimización Bayesiana| --------+                      ^
|     |                           +----------------------+                                |
|     |                                                                                   |
|     |-----------------------------------------------------------------------------------+
+-----+
```

## Random forest
**Como obtengo árboles distintos**
* Perturbo el dataset:
    * Misma cantidad de registros.
    * Toma N registros del dataset original con repetición.
* Perturbo el algoritmo del árbol:
    * En cada nodo:
        * No uso todas las variables.
        * Elijo MTRY variables al azar de las m que tiene el dataset. MTRY < sqrt(m)

## Gradient boosting
Nodo de la siguiente manera
```
+-------+
|  POS  |
|  NEG  |
+-------+
```
$$\dfrac{POS \pm \alpha}{POS + NEG + \lambda}$$

Sea:
* $\alpha$: regularización $L_{1}$
* $\lambda$: regularización $L_{2}$

## Clase 10/10
Undersampling de los negativos {BAJA+1, CONTINUA}
Tarea para hogar
| Algoritmo | % Negativos | Tiempo | $$$  | Ganancia | Trabaja con clase |
| --------- | ----------- | ------ | ---- | -------- | ----------------- |
| RPART     | 10%         | -      | -    | -        | Ternaria          |
| RANGER    | 10%         | -      | -    | -        | Ternaria          |
| XGBOOST   | 10%         | -      | -    | -        | Binaria           |
| LIGHTGBM  | 10%         | -      | -    | -        | Binaria           |
| LIGHTGBM  | 100%        | -      | -    | -        | Binaria           |

$$GAN\_TOTAL = 19500*BAJA\_2 - 500(BAJA\_1 + CONTINUA)$$
$$GAN\_UNDERSAMPLING = 19500*BAJA\_2 - 5000(BAJA\_1 + CONTINUA)$$
```
DATASET ORIGINAL                 |    DATASET UNDERSAMPLING 10%
---------------------------------+--------------------------------------
+---------+    10/10+390         |    +---------+   10/10+39
| POS 10  |    0.025             |    | POS 12  |   0.22
| NEG 390 |                      |    | NEG 39  |
+---------+                      |    +---------+
GANANCIA: 0                      |  GANANCIA: 214000
```

## Clase 17/10
Es factible entrenar con:
* 1 = {BAJA+2, BAJA+1}, BAJA+3, BAJA+4
* 0 = {CONTINUA}

BAJA+1 > BAJA+2 > BAJA+3 > BAJA+4 > ... > CONTINUA

```
REALIDAD                         |    TRANSFORMADO
---------------------------------+--------------------------------------
+---------+    p(pos)=0.023      |    +---------+   p(pos)=0.05
| B+2 9   |                      |    | POS 20  |   
| B+1 11  |                      |    | NEG 380 |
| CON 380 |                      |    +---------+
+---------+                      |    
GANANCIA: -20000                 |  GANANCIA: xxxx
```
Existe diferencia entre los BAJA+1 y BAJA+2?. Armando un modelo nuevo con solo los BAJA+1 y BAJA+2, puedo distinguirlos con respecto a sus atributos?.
La respuessta en NO. Armar modelo para diferenciarlo usando lightgbm con optimización bayesiana. Esto es para ver si la clase es separable. En caso de encontrar esto ayudará a mejorar la ganancia.

## Clase 24/10
Tarea hogar 9 con la linea de muerte ya subida en R/lineademuerte
Dropbox en R/FeatureEngineering
|              | Sin variables nuevas | Con variables nuevas         |
| ------------ | -------------------- | ---------------------------- |
| Sin historia | _DIAS                | _EXT con fe_presente.r       |
| Con historia | _HIST fe_historia.r  | _EXT_HISTORIA fe_todoenuno.r |

Ejecutar fe_todoenuno.r con el agregado de más variables

### Linea de muerte
* usa PAQUETE_PREMIUM_HIST.TXT
* usa XGBOOST
* entrena sobre todos los datos, no usa under sampling (8 vcpu y 128 gb ram)
* ventana de 10 meses (encontrado por optimización bayesiana)
    * Entrena 2019201807, ...., 201904
    * Aplica a 201906
* solo se usan tres parámetros del XGBOOST (encontrados por optimización bayesiana)
    * NROUNDS (300)
    * ETA (0.02)
    * COLSAMPLE_BYTREE (0.6)

Al aplicar XGBOOST con esos parámetros la los 10 meses se genera un modelo, al hacer el predict en 201906 se obtienen 3 archivos

Usar los mismos hiperparámetros

| train_desde | train_hasta | mes_aplicacion | ganancia |
| ----------- | ----------- | -------------- | -------- |
| 201707      | 201804      | 201806         | ?        |
| ...         | ...         | ...            | ...      |
| 201803      | 201812      | 201902         | ?        |
| 201804      | 201901      | 201903         | ?        |
| 201805      | 201902      | 201904         | ?        |

Hay que tener un modelo que le gane en todos los casos anteriores. Ganarle comodamente, o sea, más de 200000 en cada mes.

#### Intentos de ganarle a la línea de muerte
1. Fallido
    * dataset: PAQUETE_PREMIUM_DIAS.TXT
2. Solo gana 1 mes
    * dataset: PAQUETE_PREMIUM_HIST.TXT
    * undersampling: negativos 10%
    * método: lightgbm
3. Gana algunos más
    * dataset: PAQUETE_PREMIUM_HIST
    * undersampling: no
    * uso todos los datos
    * método: lightgbm
4. **Usar directamente el dataset con las variables nuevas!!!!!**.