library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
DataSet1 <- DATASETMa
colnames(DataSet1)
summary(DataSet1)
pairs(
  DataSet1[c("TEMP","HUMEDAD","DIS_UP","PPM")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet1$AMBIENTE)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor3 = rojo, Sensor 4 = naranja"
)

##----------------------Arbol de decision-----------------
predictors <- colnames(DataSet1)[-7]
data.samples <- sample(1:nrow(DataSet1),
                       nrow(DataSet1) *0.7, replace = FALSE)
sample(DataSet1)
training.data <- DataSet1[data.samples,c(predictors,"AMBIENTE") ]
test.data <- DataSet1[-data.samples,c(predictors,"AMBIENTE") ]
#Arbol de decision
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
#Modelo Lineal------------------------------------------------------------
modelo_multilineal <- lm(DataSet1$AMBIENTE ~ DataSet1$TEMP + DataSet1$HUMEDAD + DataSet1$DIS_UP +
                           DataSet1$PPM, data = training.data)
summary(modelo_multilineal)
prediccionlm <- predict(modelo_multilineal,test.data)
prediccionlm
RMS1 = data.frame(prediccion = prediccionlm
                         ,actual =DataSet1
                         ,RSE = sqrt((prediccionlm-DataSet1$AMBIENTE)^2)
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
