# XGBoost
dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[, 4:14]

# Codificar variables
# unique(dataset[c("Gender")]) # Permite ver todos los valores unicos de cada columna
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c("France", "Spain", "Germany"),
                                      labels = c(0,1,2)))

dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c("Female", "Male"),
                                   labels = c(0,1)))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
library(caTools)

split = sample.split(dataset$Exited,SplitRatio = 0.8)
train_set = subset(dataset, split = TRUE)
test_set = subset(dataset, split = FALSE)

# Ajustar XGBoost al conjunto de entrenamiento
#install.packages("xgboost")
library(xgboost)
classifier = xgboost(data = as.matrix(train_set[, -11]),
                     label = train_set$Exited,
                     nrounds = 10)

# Aplicar algoritmo de k-fold cross validation
library(caret)
folds = createFolds(train_set$Exited, k = 10)
cv = lapply(folds, function(x){
  train_fold = train_set[-x, ]
  test_fold = train_set[-x, ]
  classifier = xgboost(data = as.matrix(train_set[, -11]),
                       label = train_set$Exited,
                       nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[,-11]))
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[1,2])
  return(accuracy)
}) # lapply = aplicar a una lista una acción a todos los valores
accuracy = mean(as.numeric(cv))
accuracy_sd = sd(as.numeric(cv))