dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, c("Level","Salary")]

#library (caTools) 
#set.seed(123)
#split = sample.split(dataset$Salary, Split)
#training_data = subset(dataset, split = TRUE)
#test_data = subset(dataset, split = FALSE)
library(e1071)
library (ggplot2)

regression = svm(formula = Salary ~ .,
                 data = dataset,
                 type = "eps-regression",
                 kernel = "radial")

y_pred = predict(regression, newdata = data.frame(Level = 6.5))

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regression, newdata = dataset)), color = "blue") +
  ggtitle("Predición SVR dependiendo del sueldo") +
  xlab("Nivel del empleado") +
  ylab("Sueldo en dolares")
