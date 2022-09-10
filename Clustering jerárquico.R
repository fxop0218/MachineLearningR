# Clustering jerárquico

# Importar el dataset 

dataset <- read.csv("Mall_Customers.csv")
X <- dataset[, 4:5]

# Utilizar endograma para encontrar el número óprimo de clusters
dendogram <- hclust(dist(X, method = "euclidean"),
                   method = "ward.D")

# Visualización endograma
plot(dendogram,
     main = "Dendrograma",
     xlab = "Clientes del centro comercial",
     ylab = "Distancia")

# Ajustar el clustering jerárquico

hc <- hclust(dist(X, method = "euclidean"),
            method = "ward.D")

hc_predict <- cutree(hc, k = 5)  # Cortar el endograma en el numero de k (Klustes) donde queiro cortar

# Visualización de los clusters (Solo se puede con datos en dos dimensiones)
library(cluster)
clusplot(X, hc_predict,
         lines = 0, # No conecta con lineas 
         shade = TRUE, # Muestra una ligera sombra
         color = TRUE, # Utiliza color si es true si es falo con escala de grises
         labels = 1, # Formato de puntos diferentes 1 = puntos, 2 numero de putos
         plotchar = FALSE,
         span = TRUE, 
         main = "Clustering de clientes",
         xlab = "Ingresos anuales",
         ylab = "Puntuación (1-100)")