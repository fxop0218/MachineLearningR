# ACP 

# Importar y preparar los datos
dataset = read.csv("Wine.csv")

library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalar las variables

train_set[,-14] = scale(train_set[,-14])
test_set[,-14] = scale(test_set[,-14])

# Proyección de los datos principales (reducir la dimensionalidad de el dataset)
library(caret)
library(e1071)

pca = preProcess(x = train_set[, -14], method = "pca", pcaComp = 2) # Se puede escoger el % de la varianza o un numero exacto de valores
train_set = predict(pca, train_set)
test_set = predict(pca, test_set)

# Movemos las columnas para que el dato a predecir quede al final 
train_set = train_set[, c(2,3,1)]
test_set = test_set[, c(2,3,1)]

# Creación del modelo de SVM

classifier = svm(formula = Customer_Segment ~ .,
                 data = train_set,
                 type = "C-classification",
                 kernel = "linear")

y_pred = predict(classifier, newdata = test_set[,-3])
# Crear matriz de confusión

cm <- table(test_set[,3], y_pred)

# Visualizar el conjunto de datos
library(ElemStatLearn)

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (ACP) (Conjunto de Entrenamiento)',
     xlab = 'CP1', ylab = 'CP2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2, "deepskyblue",
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3]==2, "blue3",
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (ACP) (Conjunto de Entrenamiento)',
     xlab = 'CP1', ylab = 'CP2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid==2, "deepskyblue",
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3]==2, "blue3",
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

