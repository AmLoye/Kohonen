dist = function(u, v){
  #calcul la distance euclidienne entre deux vecteurs u et v
  return(sqrt(sum((u-v)^2)))
}


sigma_i = 5
sigma_f = 0.2
sigma = function(t, M){
  # on fixe une valeur minimum pour éviter une division par zero dans la fonction delta
  res = max(sigma_i*((sigma_f/sigma_i)^(t/M)), 10^(-50))
  return(res)
}

delta = function(t, i, j, M){
  #la fonction de voisinnage definit de IxI dans [0, 1]
  return(exp(-(dist(i, j)^2)/(2*sigma(t, M)**2)))
}

epsilon_i = 0.1
epsilon_f = 0.005
epsilon = function(t, M){
  #le paramètre d'adaptation
  return(epsilon_i*(epsilon_f/epsilon_i)**(t/M))
}


#creation d'un jeu de vecteurs

#loi uniforme
Z = matrix(runif(2000), nrow = 2, ncol = 1000)
#vecteurs gaussiens
Y = matrix(c(rnorm(50, mean = -3)/6, rnorm(50, mean = 3)/6, rnorm(50, mean = -3)/6, rnorm(50, mean = 3)/6), nrow = 2, byrow = TRUE)

A = matrix(runif(6), nrow = 2, ncol = 3)
#Iris
# Z = matrix(c(iris$Sepal.Length, iris$Sepal.Width), nrow = 2)
