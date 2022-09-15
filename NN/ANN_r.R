# Artificial neuronal network

dataset = read.csv("Churn_Modelling.csv")
dataset = dataset[, 4:14]

# Codificar variables
# unique(dataset[c("Gender")]) # Permite ver todos los valores unicos de cada columna
dataset$Geography = as.numeric(factor(dataset$Geography,
                        levels = c("France", "Spain", "Germany"),
                        labels = c(0,1,2)))

dataset$Gender = as.numeric(factor(dataset$Gender,
                           levels = c("Female", "Male"),
                           labels = c(0,1)))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
library(caTools)

split = sample.split(dataset$Exited,SplitRatio = 0.8)
train_data = subset(dataset, split = TRUE)
test_data = subset(dataset, split = FALSE)

# Escalar los datos
train_data[,-11] = scale(train_data[,-11])
test_data[,-11] = scale(test_data[,-11])

# Crear una red neuronal
#install.packages("h2o")
library(h2o)  # Mejor paquete para creación de redes nueronales

h2o.init(nthreads = 2) # Se puede usar para computación en la nube+
classifier = h2o.deeplearning(y = "Exited",
                              training_frame = as.h2o(train_data),
                              activation = "Rectifier",
                              hidden = c(6, 12), epochs = 100,
                              train_samples_per_iteration = -2)

prob_pred = h2o.predict(classifier,
                        newdata = as.h2o(test_data[,-11]))
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred = as.vector(y_pred)
cm = table(test_data[, 11], y_pred)

# Cerrar sesión h20
h2o.shutdown()
