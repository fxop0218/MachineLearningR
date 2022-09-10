# clustering con kmeans

# Importar los datos
dataset <- read.csv("Mall_Customers.csv")
X = dataset[, 4:5]

# Metodo del codo

set.seed(6)

wcss <- vector()

for (i in 1:10){
  wcss[i] <- sum(kmeans(X, i)$withinss)
}

plot(1:10, wcss, type = "b", main = "Metodo del codo",
     xlab = "Numero de clusters (k)", ylab = "WCSS(k)")

# Aplicamos el algoritmo de k-means con k óptimo en este caso (5)

set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

# Representación de clusrers
# install.packages("cluster")
library(cluster)
clusplot(X, kmeans$cluster,
          lines = 0, # No conecta con lineas 
          shade = TRUE, # Muestra una ligera sombra
          color = TRUE, # Utiliza color si es true si es falo con escala de grises
          labels = 1, # Formato de puntos diferentes 1 = puntos, 2 numero de putos
          plotchar = FALSE,
          span = TRUE, 
          main = "Clustering de clientes",
          xlab = "Ingresos anuales",
          ylab = "Puntuación (1-100)")