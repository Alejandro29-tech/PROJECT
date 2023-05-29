library(tidyverse)
library(psych)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(class)
library(gmodels)
DataSet <- DATASETMa
colnames(DataSet)
summary(DataSet)
predictors <- colnames(DataSet)[-7]
data.samples <- sample(1:nrow(DataSet),
                       nrow(DataSet) *0.7, replace = FALSE)
sample(DataSet)
training.data <- DataSet[data.samples,c(predictors,"AMBIENTE") ]
test.data <- DataSet[-data.samples,c(predictors,"AMBIENTE") ]
pairs(
  DataSet[c("TEMP","HUMEDAD","DIS_UP","PPM")]
  ,pch=21, bg=c("black","blue","red","yellow","orange","purple","white")[unclass(DataSet$AMBIENTE)]
  ,main="Sensor1 = Negro- Sensor 2 = Azul- Sensor3 = rojo, Sensor 4 = naranja"
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
plot(RMS1)
##------------------Modelo GLM ------------------------------------------------
Modelo2 <- glm(AMBIENTE ~ TEMP + HUMEDAD + DIS_UP + PPM, data = training.data)
summary(Modelo2)
modeloPredictivo2 <- predict(Modelo2,test.data)
RMS2 = data.frame(prediccion = modeloPredictivo2
                  ,actual =test.data$AMBIENTE
                  ,RSE = sqrt((modeloPredictivo2-test.data$AMBIENTE)^2)
)
View(RMS2)
plot(RMS2)
##-------------------- Modelo KNN ----------------------------------------------
DataSet$AMBIENTE <- as.factor(DataSet$AMBIENTE)
data.samples<- sample(1:nrow(DataSet)
                      , nrow(DataSet)*0.7
                      , replace = F)

predictors <- c("TEMP","HUMEDAD","DIS_UP","PPM")

training.data <-
  DataSet[data.samples,c(predictors,"AMBIENTE"),drop=F]
test.data <-
  DataSet[-data.samples,c(predictors,"AMBIENTE"),drop=F]

ctrl <- trainControl(method = "cv",p=7)
knnFit <- train(AMBIENTE ~ TEMP + HUMEDAD + DIS_UP + PPM
                , data = training.data
                , method = "knn",trControl=ctrl
                , preProcess = c("center","scale")
                , tuneLength = 20)
knnFit
plot(knnFit)

knnPredict <- predict(knnFit,newdata = test.data)
caret::confusionMatrix(knnPredict,test.data$AMBIENTE)

##----------------------Arbol de decision-----------------


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



