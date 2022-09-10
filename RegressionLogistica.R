dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[c("Age","EstimatedSalary", "Purchased")]
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Escalar las variables

train_set[c("Age","EstimatedSalary")] = scale(train_set[c("Age","EstimatedSalary")])
test_set[c("Age","EstimatedSalary")] = scale(test_set[c("Age","EstimatedSalary")])

# Creación del modelo de regresión logistica
classifier = glm(formula = Purchased ~ .,
                 data = train_set, 
                 family = binomial) # Necesario para la regressión logistica

# Predicción resultados con el conjunto de testing

prob_pred <- predict(classifier,
                     newdata = test_set[c("Age","EstimatedSalary")])  # La información que salta es la probabilidad de compra del coche

# Transformar probabilidades en 0 no compra o 1 compra

y_pred = ifelse(prob_pred>0.5, 1, 0)

# Crear matriz de confusión

cm <- table(test_set[,3], y_pred)

# Visualizar el conjunto de datos
library(ElemStatLearn)
ggplot(df, aes(x=var2, y=var1)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))
