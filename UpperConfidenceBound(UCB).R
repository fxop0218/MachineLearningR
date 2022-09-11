# Upper confidence Bound

# Importar los datos

dataset = read.csv("Ads_CTR_Optimisation.csv")


# Seleción aleatoria que nos permite ver el exito que tendria si escogemos el anuncio de forma random
# Es decir no maximiaza el mejor anuncio

N = 10000
d = 10

#ads_selected = integer(0)
#total_reward = 0

#for (n in 1:N){
#  ad = sample(1:10, 1)
#  ads_selected = append(ads_selected, ad)
#  reward = dataset[n, ad]
#  total_reward = total_reward + reward
#}

#hist(ads_selected,
#     col = "green",
#     main = "Histograma de ads selecionados",
#     xlab = "ads",
#     ylab = "seleciones")

# Como se puede ver es muy uniforme (poco optimizado)

# Implementamso UCB

number_of_selections = integer(d)
sums_of_rewards = integer(d)
ads_selected = integer(0)
total_reward = 0
for(n in 1:N){
  max_upper_bound = 0
  ad = 0
  for(i in 1:d){
    if(number_of_selections[i]>0){
      average_reward = sums_of_rewards[i] / number_of_selections[i]
      delta_i = sqrt(3/2*log(n)/number_of_selections[i])
      upper_bound = average_reward + delta_i
    }else{
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  number_of_selections[ad] = number_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

# Visualización de resultados - Histograma
hist(ads_selected,
     col = "lightblue",
     main = "Histograma de los Anuncios",
     xlab = "ID del Anuncio",
     ylab = "Frecuencia absoluta del anuncio")

# Como se puede ver en el actual, el anuncio que se a optimizado es el 5