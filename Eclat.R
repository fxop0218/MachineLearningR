# Eclat

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

# Entrenar algorito eclat con el dataset
# Clacular el soport con 3 ventas diarias formula = (3*7/7500) = 0.028// 7500 = cestas de la compra
rules = eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))

# Visualizació de los resultados
inspect(sort(rules, by = "support")[1:10])
plot(rules, method = "graph", engine = "htmlwidget") # Visualización en forma de mapa
