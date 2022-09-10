dataset = read.csv("Position_Salaries.csv")
dataset = dataset[, c("Level","Salary")]

#library (caTools)
#set.seed(123)
#split = sample.split(dataset$Salary, Split)
#training_data = subset(dataset, split = TRUE)
#test_data = subset(dataset, split = FALSE)
lin_reg = lm(formula = Salary ~ .,
             data = dataset)

summary(lin_reg)

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3 # Añadir tantos como quieras, puede dar valores muy desorbitados si te pasas

poly_reg = lm(formula = Salary ~ .,
             data = dataset)

summary(poly_reg)

library (ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)), color = "blue") +
  ggtitle("Predición lineal dependiendo del sueldo") +
  xlab("Nivel del empleado") +
  ylab("Sueldo en dolares")

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)), color = "blue") +
  ggtitle("Predición polinomica dependiendo del sueldo") +
  xlab("Nivel del empleado") +
  ylab("Sueldo en dolares")

# Predición de un solo valor

y_pred = predict(lin_reg, newdata = data.frame(Level = 6.5))

y_pred_poly = predict(poly_reg, newdata = data.frame(Level = 6.5,
                                                     Level2 = 6.5^2,
                                                     Level3 = 6.5^3))
