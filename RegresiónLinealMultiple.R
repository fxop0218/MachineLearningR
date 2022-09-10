dataset = read.csv("50_Startups.csv")

dataset$State = factor(dataset$State,
                       levels = c("New York", "California", "Florida"),
                       labels = c(0,1,2))

library(caTools)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Regresión lineal multiple
# EL . hace referencia a todas las demas columnas
regression = lm(formula = Profit ~ .,
                data = training_set)

summary(regression)

# Predecir el modelo
y_pred = predict(regression, newdata = test_set)


# Construir el modelo optimo
SL = 0.05 # Eliminar todos los que tiene un P valor menos que SL
regression = lm(formula = Profit ~ .,
                data = dataset)

summary(regression)

regression = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = dataset)

summary(regression)

# Eliminamos administration
regression = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)

summary(regression)

# Eliminamos Marketing.Spend
regression = lm(formula = Profit ~ R.D.Spend,
                data = dataset)

summary(regression)

# eliminación hacia atras automatica 
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)