# Algoritmo a priori

# Preparación de los datos
#install.packages("arulesViz")
# install.packages("arules")
library(arules)
library(arulesViz)

dataset = read.csv("Market_Basket_Optimisation.csv", header = FALSE) # Tenemos que poner header = a falso, ya que no existe una cabecera
dataset = read.transactions("Market_Basket_Optimisation.csv", sep = ",", rm.duplicates = TRUE) # rm.duplicates elimina los valores duplicados

# Mostrar información visual sobre el dataset
summary(dataset)
itemFrequencyPlot(dataset, topN = 50)# Muestra los valores mas comprados

# Entrenar algorito Apriori con el dataset
# Clacular el soport con 3 ventas diarias formula = (3*7/7500) = 0.028// 7500 = cestas de la compra
rules = apriori(data = dataset, parameter = list(support = 0.003, confidence = 0.2)) # 0.2 reglas de asociación que suceden el 20% de las veces

# Visualizació de los resultados
inspect(sort(rules, by = "lift")[1:10])
plot(rules, method = "graph", engine = "htmlwidget") # Visualización en forma de mapa
