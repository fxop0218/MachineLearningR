# ACP 

# Importar y preparar los datos
dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[c("Age", "EstimatedSalary", "Purchased")]
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalar las variables

train_set[,-3] = scale(train_set[,-3])
test_set[,-3] = scale(test_set[,-3])

# Aplcicar Kernel ACP
# install.packages("kernlab")
library(kernlab)
kpca = kpca (~., data = train_set[, -3],
             kernel = "rbfdot", features = 2)

train_set_pca = as.data.frame(predict(kpca, train_set))
test_set_pca = as.data.frame(predict(kpca, test_set))

train_set_pca$Purchased = train_set$Purchased
test_set_pca$Purchased = test_set$Purchased

# Movemos las columnas para que el dato a predecir quede al final 
#train_set = train_set[, c(2,3,1)]
#test_set = test_set[, c(2,3,1)]

# Creación del modelo de regresion logistica

classifier = glm(formula = Purchased ~.,
                 data = train_set_pca,
                 family = binomial)

prob_pred = predict(classifier, newdata = test_set_pca[,-3], type = "response")
y_pred = ifelse(prob_pred > 0.5 ,1,0)
# Crear matriz de confusión

cm <- table(test_set[,3], y_pred)

# Visualizar el conjunto de datos
library(ElemStatLearn)

set = test_set_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
# Solo cuando son prob
prob_pred = predict(classifier, newdata = grid_set, type = "response")
y_grid = ifelse(prob_pred > 0.5 ,1,0)

plot(set[, -3],
     main = 'regresión lineal (ACP) (Conjunto de Entrenamiento)',
     xlab = 'CP1', ylab = 'CP2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg =ifelse(set[, 3] == 1, 'green4', 'red3'))



set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
# Solo cuando son prob
prob_pred = predict(classifier, newdata = grid_set, type = "response")
y_grid = ifelse(prob_pred > 0.5 ,1,0)

plot(set[, -3],
     main = 'regresión lineal (ACP) (Conjunto de Entrenamiento)',
     xlab = 'CP1', ylab = 'CP2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

