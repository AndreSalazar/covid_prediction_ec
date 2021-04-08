#Borrado de variables
rm(list = ls())
library(stringr)

#Medir el tiempo - inicio
t <- proc.time()

#Lectura de dataset de muertes
datasetMuertes <- read.csv("https://raw.githubusercontent.com/andrab/ecuacovid/master/datos_crudos/muertes/provincias.inec.csv", header = TRUE, fileEncoding="utf-8")

#Selecciona una ciudad
#datasetMuertes <- datasetMuertes[datasetMuertes$inec_provincia_id == 17, ]

#Verificamos que no hayan datos faltantes NA
apply(datasetMuertes,2,function(x) sum(is.na(x)))


#Split a la columna de fecha para obtener el dia, mes y aÃ±o
datasetMuertes <- data.frame(datasetMuertes, (str_split_fixed(datasetMuertes$created_at, "/", 3)))

#Transformo el dia, mes y aÃ±o a numerico
dia <- as.numeric(datasetMuertes$X1)
mes <- as.numeric(datasetMuertes$X2)
anio <- as.numeric(datasetMuertes$X3)
datasetMuertes <- data.frame(datasetMuertes, dia, mes, anio)

#Elimino columnas cuyos datos no van a ser necesarios para el modelo (redundancias)
borrar <- c("provincia", "created_at", "X1", "X2", "X3", "anio")
datasetMuertes <- datasetMuertes[ , !(names(datasetMuertes) %in% borrar)]


###############################################################
###############################################################
###############################################################

#Selecciona una ciudad
train <- datasetMuertes[datasetMuertes$mes > 2 & datasetMuertes$mes <8 & datasetMuertes$dia<10, ]
test <- datasetMuertes[datasetMuertes$mes ==7 & datasetMuertes$dia==11, ]

#Elimino columnas cuyos datos no van a ser necesarios para el modelo (redundancias)
borrar <- c("provincia", "created_at", "X1", "X2", "X3", "anio")
train <- train[ , !(names(train) %in% borrar)]

#Elimino columnas cuyos datos no van a ser necesarios para el modelo (redundancias)
borrar <- c("provincia", "created_at", "X1", "X2", "X3", "anio")
test <- test[ , !(names(test) %in% borrar)]


write.csv(test[, c(1,2,3,4,5,6,7)], file = "test.csv", row.names = FALSE)

#Pre-procesamiento de datos
#Normalizamos los datos, mediante el metodo de la escala de minimos y maximos, en el intervalo [0,1]
trainmaxs <- apply(train, 2, max)
trainmins <- apply(train, 2, min)

testmaxs <- apply(test, 2, max)
testmins <- apply(test, 2, min)

#Escalamos los datos
trainScaled <- as.data.frame(scale(train, center = trainmins, scale = trainmaxs - trainmins))
testScaled <- as.data.frame(scale(test, center = testmins, scale = testmaxs - testmins))


#Entrenamiento de la red con 4 y 3 capas ocultas 
library(neuralnet)
columns <- names(trainScaled)
formula <- as.formula(paste("total ~", paste(columns[!columns %in% "total"], collapse = " + ")))
redNeuronal <- neuralnet(formula,data=trainScaled,hidden=c(4,3),linear.output=TRUE)

#Visualizamos el modelo grafico
plot(redNeuronal)

#PredicciÃ³n con una red neuronal
library(neuralnet)
prediccion.redNeuronal <- neuralnet::compute(redNeuronal,testScaled[,1:7])

#Como el resultado de la tred neuronal esta normalizado (escalado)
#se hace el proceso inverso (quitar escala) para comparar
prediccion.redNeuronal_ <- prediccion.redNeuronal$net.result*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)
test.r <- (testScaled$total)*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)

prediccion <- round(test.r)


#Calculamos el valor MSE para la red neuronal
MSE.redNeuronal <- sum((test.r - prediccion.redNeuronal_)^2)/nrow(testScaled)

#Compararmos los dos resultados ECM (lineal y red neuronal)
print(paste(MSE.lm,MSE.redNeuronal))








###############################################################
###############################################################
###############################################################





#Dividimos el conjunto de datos en entrenamiento y pruebas
index <- sample(1:nrow(datasetMuertes),round(0.80*nrow(datasetMuertes)))
train <- datasetMuertes[index,]
test <- datasetMuertes[-index,]

#Ajustamos un modelo de regresión lineal y lo probamos
lm.fit <- glm(total~., data=train)
summary(lm.fit)

#Predecimos los datos desde el modelo lineal ajustado
pr.lm <- predict(lm.fit,test)

#Prueba del MSE
MSE.lm <- sum((pr.lm - test$total)^2)/nrow(test)   #Mse del metodo lineal

#-------------------------------------------------------------------------------
#Ajuste para una red neuronal

#Pre-procesamiento de datos
#Normalizamos los datos, mediante el metodo de la escala de minimos y maximos, en el intervalo [0,1]
maxs <- apply(datasetMuertes, 2, max)
mins <- apply(datasetMuertes, 2, min)

#Escalamos los datos
scaled <- as.data.frame(scale(datasetMuertes, center = mins, scale = maxs - mins))

#Dividimos los datos escalados en datos de entrenamiento y de test
trainScaled <- scaled[index,]
testScaled <- scaled[-index,]

#Entrenamiento de la red con 4 y 3 capas ocultas 
library(neuralnet)
columns <- names(trainScaled)
formula <- as.formula(paste("total ~", paste(columns[!columns %in% "total"], collapse = " + ")))
redNeuronal <- neuralnet(formula,data=trainScaled,hidden=c(4,3),linear.output=TRUE)

#Visualizamos el modelo grafico
plot(redNeuronal)

#PredicciÃ³n con una red neuronal
library(neuralnet)
prediccion.redNeuronal <- neuralnet::compute(redNeuronal,testScaled[,1:7])

#Como el resultado de la tred neuronal esta normalizado (escalado)
#se hace el proceso inverso (quitar escala) para comparar
prediccion.redNeuronal_ <- prediccion.redNeuronal$net.result*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)
test.r <- (testScaled$total)*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)

#Calculamos el valor MSE para la red neuronal
MSE.redNeuronal <- sum((test.r - prediccion.redNeuronal_)^2)/nrow(testScaled)

#Medir el tiempo - Final
proc.time()-t

#Compararmos los dos resultados ECM (lineal y red neuronal)
print(paste(MSE.lm,MSE.redNeuronal))

##Conclusion
#Aparentemente la red neuronal está haciendo un mejor trabajo que el modelo lineal en la predicción
#del total de casos de muertes por Covid-19. Esto por ahora dependen de como esta dividido el conjunto
#de datos (train y test)


#Se hacen las pruebas ahora por validacion cruzada para obtener mas confianza de los resultados
# Plot de predicciones
par(mfrow=c(1,2))

plot(test$total,prediccion.redNeuronal_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red')

plot(test$total,pr.lm,col='blue',main='Real vs predicted LM',pch=18, cex=0.7)
abline(0,1,lwd=2)
#legend('bottomright',legend='LM',pch=18,col='blue', cex=.95)

#Comparamos las visualizaciones para NN y LM
plot(test$total,prediccion.redNeuronal_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$total,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))



#Evaluamos el modelo LM y NN mediante validacion cruzada
library(boot)
set.seed(200)

#Validación cruzada del modelo lineal
lm.fit <- glm(total~.,data=datasetMuertes)
cv.glm(datasetMuertes,lm.fit,K=10)$delta[1]

#Validación cruzada para la red neuronal
set.seed(450)
cv.error <- NULL
k <- 10

for(i in 1:k){
  index <- sample(1:nrow(datasetMuertes),round(0.9*nrow(datasetMuertes)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  redNeuronal <- neuralnet(formula,data=train.cv,hidden=c(5,2),linear.output=T)
  
  prediccion.redNeuronal <- neuralnet::compute(redNeuronal,test.cv[,1:7])
  prediccion.redNeuronal <- prediccion.redNeuronal$net.result*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)
  
  test.cv.r <- (test.cv$total)*(max(datasetMuertes$total)-min(datasetMuertes$total))+min(datasetMuertes$total)
  
  cv.error[i] <- sum((test.cv.r - prediccion.redNeuronal)^2)/nrow(test.cv)
  
}

# Promedio del MSE
mean(cv.error)

# MSE de la valicadion cruzada
cv.error


# Plot para lps resulatdos de la validacion cruzada
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


redNeuronal.5.2 <- neuralnet(formula
                           , data=trainScaled
                           , hidden=c(5,2)
                           , linear.output=TRUE)

redNeuronal.8 <- neuralnet(formula
                         , data=trainScaled
                         , hidden=8
                         , linear.output=TRUE)


library(NeuralNetTools)

#Red neuronal con 5 y 2 capas ocultas
plotnet(redNeuronal.5.2)

#Red neuronal con 8 capas ocultas
plotnet(redNeuronal.8)
