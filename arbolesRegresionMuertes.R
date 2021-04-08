#Borrado de cache
rm(list = ls())

#Medir el tiempo - inicio
t <- proc.time()

#Ruta para almacenar los resultados
setwd("C:/ejercicioR")

#Librerias a usar dentro del script
library(ggplot2)
library(caret)
library(stringr)

###########################
#Arbol de decision simple
###########################

#Leer el archivo desde la web directamente
datasetMuertesOriginal <- read.csv("https://raw.githubusercontent.com/andrab/ecuacovid/master/datos_crudos/muertes/provincias.inec.csv", header = TRUE, fileEncoding="utf-8")

#Filtra solo datos pertenecientes a una ciudad - "Pichincha"
#datasetMuertes <- datasetMuertesOriginal[datasetMuertesOriginal$inec_provincia_id == 17, ]

#Transformo el atributo de fecha "created_at" a atributos individuales de dia, mes y a�o
datasetMuertes <- data.frame(datasetMuertesOriginal, (str_split_fixed(datasetMuertesOriginal$created_at, "/", 3)))

#Cada nuevo atributo lo formateo a variable numerica
dia <- as.numeric(datasetMuertes$X1)
mes <- as.numeric(datasetMuertes$X2)
anio <- as.numeric(datasetMuertes$X3)

datasetMuertes <- data.frame(datasetMuertes, dia, mes, anio)

#Borrar variables de fechas no necesarias despues de la transformacion
borrarColumnas <- c("provincia", "created_at", "X1", "X2", "X3")  
datasetMuertes <- datasetMuertes[ , !(names(datasetMuertes) %in% borrarColumnas)]

#Verificar la estructura del dataset
str(datasetMuertes)

#Resumen del dataset
summary(datasetMuertes)

#Obtengo el numero de columnas
ncol(datasetMuertes)

#Obtengo el numero de filas
nrow(datasetMuertes)

#Nombre de los atributos del dataset
names(datasetMuertes)

#Resumen de los atributos y datos del dataset
summary(datasetMuertes)

#Estructura del dataset
str(datasetMuertes)

#Valida si hay datos nulos
is.null(datasetMuertes)

###############################
#Analisis descriptivo grafico
###############################

#Grafica de comportamiento
ggplot(datasetMuertes, aes(x = dia, y = total)) +
        geom_point()  +
        facet_wrap(~ mes, ncol = 2)

#Matriz de correlaci�n entre variables
correlationMatrix <- cor(datasetMuertes[,])

#Borrar algunas variables
borrar <- c("anio") #"inec_provincia_id", "poblacion", "lat", "lng"
datasetMuertes <- datasetMuertes[ , !(names(datasetMuertes) %in% borrar)]

#Matriz de correlaci�n entre variables
correlationMatrix <- cor(datasetMuertes[,])
print(correlationMatrix)

# Encontrar atributos altamente correlacionados (idealmente >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)

#Covarianza. Si dos variables son independientes, su covarianza es cero
covMatrix = cov(datasetMuertes[,])
covMatrix[1,2]
covMatrix[2,3]

#Verifico la estructura del dataset final y que informacion es nula
str(datasetMuertes)
which(is.na(datasetMuertes))
sum(is.na(datasetMuertes))


library(tree)
train <- sample(1:nrow(datasetMuertes), round(0.80*nrow(datasetMuertes)))
datasetMuertes.train = datasetMuertes[train, ]
datasetMuertes.test = datasetMuertes[-train, ]
tree.datasetMuertes <- tree(total ~ . , datasetMuertes.train)

par(mfrow=c(1,1))
plot(datasetMuertes)
text(tree.datasetMuertes, pretty = 0)

cv.datasetMuertes <- cv.tree(tree.datasetMuertes, K = 10)
plot(cv.datasetMuertes$size, cv.datasetMuertes$dev, type = 'b')

#podar el Arbol
prune.datasetMuertes <- prune.tree(tree.datasetMuertes, best = 5)
plot(prune.datasetMuertes)
text(prune.datasetMuertes, pretty = 0)

#Predicción del árbol sin podar
datasetMuertes.prediction <- predict(tree.datasetMuertes, newdata = datasetMuertes[-train,])
datasetMuertes.test <- datasetMuertes[-train, "total"]
plot(datasetMuertes.prediction, datasetMuertes.test, main = "arbol sin podar")
abline(0, 1)

#Predicción árbol podado
datasetMuertes.prediction.prune <- predict(prune.datasetMuertes, newdata = datasetMuertes[-train, ])
plot(datasetMuertes.prediction.prune, datasetMuertes.test, main = "arbol podado")
abline(0, 1)

# Mean Square Error (MSE) - Sin podar da mejores resultados
mean((datasetMuertes.prediction - datasetMuertes.test)^2)
mean((datasetMuertes.prediction.prune  - datasetMuertes.test)^2)

#Conclusion
#Como conclusion de este metodo se puede ver que el arbol sin podar da mejores resultados

#Medir el tiempo - inicio
proc.time()-t

#################################
#Random Forest regresion 
#################################

#install.packages("randomForest")
library(randomForest)
library(gmodels)

#Medir tiempo - inicio
t <- proc.time()


datasetMuertes.test1 = datasetMuertes[-train, ]

columnas <-c("total", "dia","mes")

datasetMuertes.model <- randomForest(x = datasetMuertes.train[, columnas],
                                       y = datasetMuertes.train$total,
                                       data = datasetMuertes.train, 
                                       ntree = 500,
                                       do.trace= T)

#Out-Of-bag Error 
#datasetMuertes.model
plot(datasetMuertes.model)

#Variables más importantes
varImpPlot(datasetMuertes.model)

#predicción de test
datasetMuertes.predictionRF <- predict(datasetMuertes.model, datasetMuertes.test1)
#head(datasetMuertes.predictionRF)
#head(datasetMuertes.test1$total)

#Entrenar un modelo con 100 árboles
datasetMuertes.model2 <- randomForest(x = datasetMuertes.train[, columnas],
                                        y = datasetMuertes.train$total,
                                        data = datasetMuertes.train, 
                                        ntree = 100,
                                        do.trace= T)

#Dibujar el error
#datasetMuertes.model2
plot(datasetMuertes.model2)

# predecir con el modelo de 100 años
datasetMuertes.predictionRF2 <- predict(datasetMuertes.model2, datasetMuertes.test1)
#head(datasetMuertes.predictionRF)
#head(datasetMuertes.test1$total)

# Mean Square Error (MSE)
mean((datasetMuertes.predictionRF - datasetMuertes.test)^2)
mean((datasetMuertes.predictionRF2  - datasetMuertes.test)^2)

#Medir tiempo - final
proc.time()-t

#SEGUNDA PARTE
#Discretizar la variable total
datasetMuertes2 <- datasetMuertes
totalClass <- ifelse(datasetMuertes2$total <= 100, 1, 2)
datasetMuertes2 <- data.frame(datasetMuertes2, totalClass)
summary(datasetMuertes2)
str(datasetMuertes2)

#Convertirla como factor
datasetMuertes2$totalClass <- as.factor(datasetMuertes2$totalClass)

#Datos de entrenamiento y de test
datasetMuertes2.train <- sample(1:nrow(datasetMuertes2),nrow(datasetMuertes2)/2)
datasetMuertes2.test <- datasetMuertes2[-datasetMuertes2.train, ]
totalClass.test <- datasetMuertes2$totalClass[-datasetMuertes2.train]

#Entrenar
datasetMuertes2.tree <- tree(totalClass ~ .-total , datasetMuertes2[datasetMuertes2.train, ])
summary(datasetMuertes2.tree)

#Predecir
datasetMuertes2.predict <- predict(datasetMuertes2.tree, datasetMuertes2.test, type = "class")
print(datasetMuertes2.predict)

#Matriz de confusion
table(datasetMuertes2.predict, totalClass.test)
prop.table(table(datasetMuertes2.predict, totalClass.test), margin = 2)

#Analizar si podar el arbol
datasetMuertes2.tree2 <- cv.tree(datasetMuertes2.tree, FUN = prune.misclass)
names(datasetMuertes2.tree2)
datasetMuertes2.tree2
plot(datasetMuertes2.tree2$size, datasetMuertes2.tree2$dev, type = 'b')

#Podado del arbol
datasetMuertes2.tree2.prune <- prune.misclass(datasetMuertes2.tree, best = 2)

plot(datasetMuertes2.tree2.prune)
text(datasetMuertes2.tree2.prune, pretty = 0)

#Prediccion
datasetMuertes2.predict.prune <- predict(datasetMuertes2.tree2.prune, datasetMuertes2.test, type = "class")

#Matriz de confusion
table(datasetMuertes2.predict.prune, totalClass.test)
prop.table(table(datasetMuertes2.predict.prune, totalClass.test), margin = 2)


#Resultados
#Conclusion: Random Forest da mejores resultados