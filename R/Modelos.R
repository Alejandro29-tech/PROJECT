library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
DataSet1 <- DATASETMa
colnames(DataSet1)
summary(DataSet1)
pairs(
  DataSet1[c("TEMP","HUMEDAD","DIS_UP","PPM")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet1$AMBIENTE)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor3 = rojo, Sensor 4 = naranja"
)
data.samples <- sample(1:nrow(DataSet1),
                       nrow(DataSet1) *0.7, replace = FALSE)

training.data <- DataSet1[data.samples, ]
test.data <- DataSet1[-data.samples, ]
#Arbol de decision
fit.rf <- randomForest(DataSet1$AMBIENTE ~ DataSet1$TEMP + DataSet1$HUMEDAD + DataSet1$DIS_UP +
                          DataSet1$PPM, data = training.data)
prediction.rf <- predict(fit.rf, test.data)
output <- data.frame(test.data$Mileage, prediction.rf)
RMSE = sqrt(sum((output$test.data.Mileage -output$prediction.rf)^2)/

              nrow(output))

RMSE
#Modelo Lineal
modelo_multilineal <- lm(DataSet1$AMBIENTE ~ DataSet1$TEMP + DataSet1$HUMEDAD + DataSet1$DIS_UP +
                           DataSet1$PPM, data = training.data)
summary(modelo_multilineal)
prediccionlm <- predict(modelo_multilineal,test.data)
prediccionlm
RMSEmodelo1 = data.frame(prediccion = prediccionlm
                         ,actual =DataSet1
                         ,RSE = sqrt((prediccionlm-DataSet1$AMBIENTE)^2)
)
View(RMSEmodelo1)
