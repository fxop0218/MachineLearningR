# Grid search
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[c("Age","EstimatedSalary", "Purchased")]

# Transform into a factor to evite the warning
dataset$Purchased = factor(dataset$Purchased,
                           labels = c("No", "Yes"),
                           levels = c(0,1))
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalar las variables

train_set[c("Age","EstimatedSalary")] = scale(train_set[c("Age","EstimatedSalary")])
test_set[c("Age","EstimatedSalary")] = scale(test_set[c("Age","EstimatedSalary")])

# Creación del modelo de SVM
library(e1071)

classifier = svm(formula = Purchased ~ .,
                 data = train_set,
                 type = "C-classification",
                 kernel = "linear")

y_pred = predict(classifier, newdata = test_set[,-3])
# Crear matriz de confusión

cm <- table(test_set[,3], y_pred)

# Aplicar algoritmo de k-fold cross validation
library(caret)
folds = createFolds(train_set$Purchased, k = 10)
cv = lapply(folds, function(x){
  train_fold = train_set[-x, ]
  test_fold = train_set[-x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = train_fold,
                   type = "C-classification",
                   kernel = "linear")
  y_pred = predict(classifier, newdata = test_fold[,-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[1,2])
  return(accuracy)
}) # lapply = aplicar a una lista una acción a todos los valores
accuracy = mean(as.numeric(cv))
accuracy_sd = sd(as.numeric(cv))

# Aply grid search to find the best parameters
# library(caret)

classifier = train(form = Purchased ~ .,
                   data = train_set, method = "svmRadial")

classifier # Show information of the classifier

# Create the model with the best params
classifier$bestTune

# Visualizar el conjunto de datos
library(ElemStatLearn)

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Conjunto de Entrenamiento)',
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))