library(tidyverse)
library(sf)

# Prueba rápida test train random
load("salidas_intermedias/dataset_completo2024-03-27.RData")

summary(dataset)

data = drop_na(dataset)
data  = data |> 
  select(-c(municipio,cod_municipio,date,YEAR,MM,DD))

data$uso_suelo = factor(data$uso_suelo)

n = nrow(data)
train_ind = sample(1:n,0.75*n)
test_ind=setdiff(1:n,train_ind)

train = st_drop_geometry(data)[train_ind,]
test = st_drop_geometry(data)[test_ind,]


heatmap(cor(train[,-20]))
max(cor(train[,-20])-diag(diag(cor(train[,-20]))))

reg <- glm(fire ~ . , data = train, family = "binomial")
summary(reg)

library(ROCR)
# test
predicciones <- predict(reg,newdata=test, tipo = "response")
roc_datos <- prediction(predicciones, test$fire)
roc_curva <- performance(roc_datos, "tpr", "fpr")
plot(roc_curva, main = "Curva ROC")

auc <- performance(roc_datos, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

#train
predicciones <- predict(reg, tipo = "response")
roc_datos <- prediction(predicciones, train$fire)
roc_curva <- performance(roc_datos, "tpr", "fpr")
plot(roc_curva, main = "Curva ROC")

auc <- performance(roc_datos, "auc")
auc <- unlist(slot(auc, "y.values"))
auc



# Prueba rápida test train año
dataset = drop_na(dataset)
dataset$fire = factor(dataset$fire)
train_ind = dataset$YEAR!=2020
test_ind=dataset$YEAR == 2020

train = st_drop_geometry(dataset)[train_ind,-c(2,3,4,5,24,25)]
test = st_drop_geometry(dataset)[test_ind,-c(2,3,4,5,24,25)]

reg <- glm(fire ~ . , data = train, family = "binomial")
summary(reg)



library(ROCR)
# test
predicciones <- predict(reg,newdata=test, tipo = "response")
roc_datos <- prediction(predicciones, test$fire)
roc_curva <- performance(roc_datos, "tpr", "fpr")
plot(roc_curva, main = "Curva ROC")

auc <- performance(roc_datos, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

#train
predicciones <- predict(reg, tipo = "response")
roc_datos <- prediction(predicciones, train$fire)
roc_curva <- performance(roc_datos, "tpr", "fpr")
plot(roc_curva, main = "Curva ROC")

auc <- performance(roc_datos, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

# No tiene sentido que el modelo funcione tan bien
