# Random forest classificator

dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[c("Age","EstimatedSalary", "Purchased")]
dataset$Purchased = factor(dataset$Purchased,levels = c(0,1))

library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalado de valores

train_set[c("Age","EstimatedSalary")] <- scale(train_set[c("Age","EstimatedSalary")]) 
test_set[c("Age","EstimatedSalary")] <- scale(test_set[c("Age","EstimatedSalary")]) 

# Creación del modelo random forest
library(randomForest)
classifier = randomForest(x = train_set[c("Age", "EstimatedSalary")],
                          y = train_set$Purchased,
                          ntree = 100)


y_pred = predict(classifier, newdata = test_set[c("Age", "EstimatedSalary")], type = "class") # Pasa de mostrate probabilidad a ponerte el mas probeble que
# Crear matriz de confusión

cm <- table(test_set[,3], y_pred)

# Visualizar el conjunto de datos
library(ElemStatLearn)

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Random forest (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Random foresst  (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))