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
datasetContagiosOriginal <- read.csv("https://raw.githubusercontent.com/andrab/ecuacovid/master/datos_crudos/positivas/provincias.inec.csv", header = TRUE, fileEncoding="utf-8")

#Filtra solo datos pertenecientes a una ciudad - "Pichincha"
#datasetContagios <- datasetContagiosOriginal[datasetContagiosOriginal$inec_provincia_id == 9, ]

#Transformo el atributo de fecha "created_at" a atributos individuales de dia, mes y a�o
datasetContagios <- data.frame(datasetContagiosOriginal, (str_split_fixed(datasetContagiosOriginal$created_at, "/", 3)))

#Cada nuevo atributo lo formateo a variable numerica
dia <- as.numeric(datasetContagios$X1)
mes <- as.numeric(datasetContagios$X2)
anio <- as.numeric(datasetContagios$X3)


datasetContagios <- data.frame(datasetContagios, dia, mes, anio)

#Borrar variables de fechas no necesarias despues de la transformacion
borrarColumnas <- c("provincia", "created_at", "X1", "X2", "X3")  
datasetContagios <- datasetContagios[ , !(names(datasetContagios) %in% borrarColumnas)]


#Verificar la estructura del dataset
str(datasetContagios)

#Resumen del dataset
summary(datasetContagios)


#Obtengo el numero de columnas
ncol(datasetContagios)

#Obtengo el numero de filas
nrow(datasetContagios)

#Nombre de los atributos del dataset
names(datasetContagios)

#Resumen de los atributos y datos del dataset
summary(datasetContagios)

#Estructura del dataset
str(datasetContagios)

#Valida si hay datos nulos
is.null(datasetContagios)

###############################
#Analisis descriptivo grafico
###############################

#Grafica de comportamiento
ggplot(datasetContagios, aes(x = dia, y = total)) +
  geom_point()  +
  facet_wrap(~ mes, ncol = 2)

#Matriz de correlaci�n entre variables
correlationMatrix <- cor(datasetContagios[,])

#Borrar algunas variables
borrar <- c("anio") #"inec_provincia_id", "poblacion", "lat", "lng"
datasetContagios <- datasetContagios[ , !(names(datasetContagios) %in% borrar)]

#Matriz de correlaci�n entre variables
correlationMatrix <- cor(datasetContagios[,])
print(correlationMatrix)

# Encontrar atributos altamente correlacionados (idealmente >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)

#Covarianza. Si dos variables son independientes, su covarianza es cero
covMatrix = cov(datasetContagios[,])
covMatrix[1,2]
covMatrix[2,3]

#Verifico la estructura del dataset final y que informacion es nula
str(datasetContagios)
which(is.na(datasetContagios))
sum(is.na(datasetContagios))


library(tree)
train <- sample(1:nrow(datasetContagios), round(0.80*nrow(datasetContagios)))
datasetContagios.train = datasetContagios[train, ]
datasetContagios.test = datasetContagios[-train, ]
tree.datasetContagios <- tree(total ~ . , datasetContagios.train)

par(mfrow=c(1,1))
plot(datasetContagios)
text(tree.datasetContagios, pretty = 0)

cv.datasetContagios <- cv.tree(tree.datasetContagios, K = 10)
plot(cv.datasetContagios$size, cv.datasetContagios$dev, type = 'b')

#podar el Arbol
prune.datasetContagios <- prune.tree(tree.datasetContagios, best = 5)
plot(prune.datasetContagios)
text(prune.datasetContagios, pretty = 0)

#Predicción del árbol sin podar
datasetContagios.prediction <- predict(tree.datasetContagios, newdata = datasetContagios[-train,])
datasetContagios.test <- datasetContagios[-train, "total"]
plot(datasetContagios.prediction, datasetContagios.test, main = "arbol sin podar")
abline(0, 1)

#Predicción árbol podado
datasetContagios.prediction.prune <- predict(prune.datasetContagios, newdata = datasetContagios[-train, ])
plot(datasetContagios.prediction.prune, datasetContagios.test, main = "arbol podado")
abline(0, 1)

# Mean Square Error (MSE) - Sin podar da mejores resultados
mean((datasetContagios.prediction - datasetContagios.test)^2)
mean((datasetContagios.prediction.prune  - datasetContagios.test)^2)

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

datasetContagios.test1 = datasetContagios[-train, ]

columnas <-c("total", "dia","mes")

datasetContagios.model <- randomForest(x = datasetContagios.train[, columnas],
                                    y = datasetContagios.train$total,
                                    data = datasetContagios.train, 
                                    ntree = 500,
                                    do.trace= T)

#Out-Of-bag Error 
#datasetContagios.model
plot(datasetContagios.model)

#Variables más importantes
varImpPlot(datasetContagios.model)

#predicción de test
datasetContagios.predictionRF <- predict(datasetContagios.model, datasetContagios.test1)
#head(datasetContagios.predictionRF)
#head(datasetContagios.test1$total)

#Entrenar un modelo con 100 árboles
datasetContagios.model2 <- randomForest(x = datasetContagios.train[, columnas],
                                y = datasetContagios.train$total,
                                data = datasetContagios.train, 
                                ntree = 100,
                                do.trace= T)

#Dibujar el error
#datasetContagios.model2
plot(datasetContagios.model2)

# predecir con el modelo de 100 años
datasetContagios.predictionRF2 <- predict(datasetContagios.model2, datasetContagios.test1)
#head(datasetContagios.predictionRF)
#head(datasetContagios.test1$total)

# Mean Square Error (MSE)
mean((datasetContagios.predictionRF - datasetContagios.test)^2)
mean((datasetContagios.predictionRF2  - datasetContagios.test)^2)

#Medir tiempo - final
proc.time()-t


#SEGUNDA PARTE
#Discretizar la variable total
datasetContagios2 <- datasetContagios
totalClass <- ifelse(datasetContagios2$total <= 100, 1, 2)
datasetContagios2 <- data.frame(datasetContagios2, totalClass)
summary(datasetContagios2)
str(datasetContagios2)

#Convertirla como factor
datasetContagios2$totalClass <- as.factor(datasetContagios2$totalClass)

#Datos de entrenamiento y de test
datasetContagios2.train <- sample(1:nrow(datasetContagios2),nrow(datasetContagios2)/2)
datasetContagios2.test <- datasetContagios2[-datasetContagios2.train, ]
totalClass.test <- datasetContagios2$totalClass[-datasetContagios2.train]

#Entrenar
datasetContagios2.tree <- tree(totalClass ~ .-total , datasetContagios2[datasetContagios2.train, ])
summary(datasetContagios2.tree)

#Predecir
datasetContagios2.predict <- predict(datasetContagios2.tree, datasetContagios2.test, type = "class")
print(datasetContagios2.predict)

#Matriz de confusion
table(datasetContagios2.predict, totalClass.test)
prop.table(table(datasetContagios2.predict, totalClass.test), margin = 2)

#Analizar si podar el arbol
datasetContagios2.tree2 <- cv.tree(datasetContagios2.tree, FUN = prune.misclass)
names(datasetContagios2.tree2)
datasetContagios2.tree2
plot(datasetContagios2.tree2$size, datasetContagios2.tree2$dev, type = 'b')

#Podado del arbol
datasetContagios2.tree2.prune <- prune.misclass(datasetContagios2.tree, best = 2)

plot(datasetContagios2.tree2.prune)
text(datasetContagios2.tree2.prune, pretty = 0)

#Prediccion
datasetContagios2.predict.prune <- predict(datasetContagios2.tree2.prune, datasetContagios2.test, type = "class")

#Matriz de confusion
table(datasetContagios2.predict.prune, totalClass.test)
prop.table(table(datasetContagios2.predict.prune, totalClass.test), margin = 2)


#Resultados
#Conclusion: Random Forest da mejores resultados