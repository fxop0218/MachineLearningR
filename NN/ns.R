dataset_o = read.delim("Restaurant_Reviews.tsv", quote = "", stringsAsFactors = FALSE)

#install.packages("tm")
#install.packages("SnowballC") # Instalación para qu elas stopwords funcionesnn
library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(dataset_o$Liked)) # Create corpus
corpus = tm_map(corpus, content_transformer(tolower)) # Transform to lower case
# as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers) # Delete number
corpus = tm_map(corpus, removePunctuation) # Delete puntuation
corpus = tm_map(corpus, removeWords, stopwords(kind = "en")) # Eliminan las stopwords (Palabras sin relevancia)
corpus = tm_map(corpus, stemDocument) # Transforma una palabra derivada en la mas simple loving = love
corpus = tm_map(corpus, stripWhitespace)

dtm = DocumentTermMatrix(corpus) # Creación de matriz dispersa
dtm = removeSparseTerms(dtm, 0.999) # Eliminar palabras que solo salen una vez

dataset = as.data.frame(as.matrix(dtm)) # Manipulamos dtm en un dataset
dataset$Liked = dataset_o$Liked

dataset$Liked = factor(dataset$Liked, levels = c(0,1))
library(caTools)
set.seed(123)
split <- sample.split(dataset$Liked, SplitRatio = 0.8)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Creación del modelo random forest
library(randomForest)
classifier = randomForest(x = train_set[,-692],
                          y = train_set$Liked,
                          ntree = 10)


y_pred = predict(classifier, newdata = test_set[,-692]) # Pasa de mostrate probabilidad a ponerte el mas probeble que
# Crear matriz de confusión

cm <- table(test_set[,692], y_pred)
