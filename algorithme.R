library(ggplot2)
source("~/Documents/euria/informatique/projet_info/init_parametres.R")
source("~/Documents/euria/informatique/projet_info/fonctions_affichage.R")



vect = function(u, d, l){ 
  # fonction qui à une unité associe le numero de son vecteur referent
  if (d == 1){
    return(u[1])
  }
  if (d == 2){
    return(u[1] + l*(u[2]-1))
  }
}


unite = function(k, d, l){
  # fonction qui au k ieme vecteur réferent associe son unité
  if(d == 1){
    return(c(k))
  }
  if(d == 2){
    a = ((k-1) %/% l)
    b = k-1 - l*a
    return(c(b+1, a+1))
  }
}


df_classes <- function(X, W, d, n) {
  
  # Fonction qui donne une classe en fonction des distances aux W_i
  f = function(x) {
    vect_dist = sapply(1:n, function(i) { dist(x, W[, i]) })
    argmin = which(vect_dist == min(vect_dist))
    return(argmin[1])
  }
  
  
  classes_X = apply(X, MARGIN = 2, f)
  df_X = data.frame(
    xcoord = X[1, ],
    ycoord = X[2, ],
    Classe = classes_X,
    Type = "données"
  )
  
  df_W = data.frame(
    xcoord = W[1, ],
    ycoord = W[2, ],
    Classe = 1:n,
    Type = "vecteur code"
  )
  
  df = rbind(df_X, df_W)
  return(df)
}

  
competition = function(x, W, d, n){
  application_partielle_dist = function(i){
    dist(x, W[, i])
  }
  vect_dist = sapply(1:n, application_partielle_dist)
  argmin = which(vect_dist == min(vect_dist))
  return(argmin[1])
}


cooperation = function(t, W, x, i_etoile, d, l, n, M){
  for (j in 1:n){
    W[,j] = W[,j] - epsilon(t, M)*delta(t, i_etoile, unite(j, d, l), M)*(W[,j] - x)
    }
  return(W)
}


algo = function(X, d, l, M, bornes, affichage){
  
  plot_donnees(X)
  
  N = ncol(X) # nombres de vecteurs de l'espace des données
  n = l**d #nombre de neurones du réseau
  
  Wx = bornes[1] + (bornes[2] - bornes[1]) * runif(n)
  Wy = bornes[3] + (bornes[4] - bornes[3]) * runif(n)
  W = matrix(c(Wx, Wy), nrow = 2, ncol = n, byrow = TRUE)
  
  if (affichage == 1){
    print(plot_grille_1(0, X, W, d, l, n))
  }

  if (affichage == 2){
    print(plot_grille_2(0, X, W, d, l, n))
  }

  k = sample(N, size = M, replace = TRUE)
    for (t in 1:M){
      x = X[, k[t]]
      i_etoile = competition(x, W, d, n)
      W = cooperation(t, W, x, unite(i_etoile, d, l), d, l, n, M)
    }
    if (affichage == 1){
      print(plot_grille_1(M, X, W, d, l, n))
    }

    if (affichage == 2){
      print(plot_grille_2(M, X, W, d, l, n))
    }
}

algo_shiny = function(X, d, l, M, affichage){
  #fonction utilisée par shiny.R
  # plot_donnees(X)
  bornes = c(0, 1, 0, 1)
  N = ncol(X) # nombres de vecteurs de l'espace des données
  n = l**d #nombre de neurones du réseau
  
  Wx = bornes[1] + (bornes[2] - bornes[1]) * runif(n)
  Wy = bornes[3] + (bornes[4] - bornes[3]) * runif(n)
  W = matrix(c(Wx, Wy), nrow = 2, ncol = n, byrow = TRUE)
  
  
  k = sample(N, size = M, replace = TRUE)
  for (t in 1:M){
    x = X[, k[t]]
    # x = runif(2, nrow = 2)
    i_etoile = competition(x, W, d, n)
    W = cooperation(t, W, x, unite(i_etoile, d, l), d, l, n, M)
  }
  if (affichage == 1){
    return(plot_grille_1(M, X, W, d, l, n))
  }
  
  if (affichage == 2){
    return(plot_grille_2(M, X, W, d, l, n))
  }
}

#algo(Y, d = 1, l = 2, M = 1000, bornes = c(0, 1, 0, 1), 1)







