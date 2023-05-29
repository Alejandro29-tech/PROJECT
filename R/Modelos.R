library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(calss)
library(gmodels)
DataSet <- DATASETMa
colnames(DataSet)
summary(DataSet)
pairs(
  DataSet[c("TEMP","HUMEDAD","DIS_UP","PPM")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet$AMBIENTE)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor3 = rojo, Sensor 4 = naranja"
)

##----------------------Arbol de decision-----------------
predictors <- colnames(DataSet)[-7]
data.samples <- sample(1:nrow(DataSet),
                       nrow(DataSet) *0.7, replace = FALSE)
sample(DataSet)
training.data <- DataSet[data.samples,c(predictors,"AMBIENTE") ]
test.data <- DataSet[-data.samples,c(predictors,"AMBIENTE") ]

fit.rf <- randomForest(AMBIENTE ~ PPM + TEMP + HUMEDAD + DIS_UP, data = training.data)


prediction.rf <- predict(fit.rf, test.data)
output <- data.frame(test.data$Mileage, prediction.rf)
RMSE = sqrt(sum((output$test.data.Mileage - output$prediction.rf)^2)/
                nrow(output))

RMSE
RMSEmodelo2 = data.frame(prediccion = prediction.rf
                         ,ahora = test.data$Mileage
                         ,RSE = sqrt((prediction.rf-test.data$mileage)^2)
                         )
#--------------------------------- Modelo Lineal---------------------------
modelo_multilineal <- lm(DataSet$AMBIENTE ~ DataSet$TEMP + DataSet$HUMEDAD + DataSet$DIS_UP +
                           DataSet$PPM, data = training.data)
summary(modelo_multilineal)
prediccionlm <- predict(modelo_multilineal,test.data)
prediccionlm
RMS1 = data.frame(prediccion = prediccionlm
                         ,actual =DataSet
                         ,RSE = sqrt((prediccionlm-DataSet$AMBIENTE)^2)
)
View(RMS1)
##------------------Modelo GLM ------------------------------------------------
Modelo2 <- glm(AMBIENTE ~ TEMP + HUMEDAD + DIS_UP +
                 PPM, data = training.data)
summary(Modelo2)
modeloPredictivo2 <- predict(Modelo2,test.data)
RMS2 = data.frame(prediccion = modeloPredictivo2
                  ,actual =test.data$AMBIENTE
                  ,RSE = sqrt((modeloPredictivo2-test.data$AMBIENTE)^2)
)
View(RMS2)
##--------------------modelo KNN----------------------------------------------
predictors <- c("TEMP","HUMEDAD","DIS_UP","PPM")

training.data <-
  DataSet[sample.index,c(predictors,"AMBIENTE"),drop=F]
test.data <-
  DataSet[-sample.index,c(predictors,"AMBIENTE"),drop=F]

prediction <- knn(train = training.data[predictors]
                  , test = test.data[predictors]
                  ,cl = training.data$AMBIENTE, k=k)
CrossTable
