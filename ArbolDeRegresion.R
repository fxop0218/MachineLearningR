dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, c("Level","Salary")]

#split = sample.split(dataset$Salary, Split)
#training_data = subset(dataset, split = TRUE)
#test_data = subset(dataset, split = FALSE)

library(rpart)
library(ggplot2)
regression = rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 1))

y_pred = predict(regression, newdata = data.frame(Level = 6.5))

X_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = X_grid, y = predict(regression, newdata = data.frame(Level = X_grid))), color = "blue") +
  ggtitle("Predición con arbol de regresión dependiendo del sueldo") +
  xlab("Nivel del empleado") +
  ylab("Sueldo en dolares")
