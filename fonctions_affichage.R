library(ggplot2)


#fonctions d'affichage
plot_donnees = function(X){
  p = ggplot(
    mapping = aes(x = X[1,], y = X[2,])) +
    geom_point() 
  print(p)
}


plot_grille_1 = function(t, X, W, d, l, n){
  if(d == 1){
    p = ggplot(
      mapping = aes(x = W[1,], y = W[2,])) +
      geom_point(mapping = aes(x = X[1,], y = X[2,]), colour = "grey85") +
      geom_point() +
      geom_segment(mapping = aes(x = W[1, 1:(n-1)], y = W[2, 1:(n-1)], xend = W[1, 2:n], yend = W[2, 2:n])) +
      labs(title = paste("grille à la", t, "ème iteration"),
           x = "x",
           y = "y")
    return(p)
  }
  if(d == 2){
    # Connexions horizontales (dans chaque rangée, sauf le bord droit)
    ind_h = which((1:n) %% l != 0)
    x_h = W[1, ind_h]
    y_h = W[2, ind_h]
    xend_h = W[1, ind_h + 1]
    yend_h = W[2, ind_h + 1]
    
    # Connexions verticales (dans chaque colonne, sauf le bord sup)
    ind_v = which((1:n) + l <= n)
    x_v = W[1, ind_v]
    y_v = W[2, ind_v]
    xend_v = W[1, ind_v + l]
    yend_v = W[2, ind_v + l]
    
    x = c(x_h, x_v)
    y = c(y_h, y_v)
    xend = c(xend_h, xend_v)
    yend = c(yend_h, yend_v)
    
    p = ggplot(
      mapping = aes(x = W[1,], y = W[2,])) +
      geom_point(mapping = aes(x = X[1,], y = X[2,]), colour = "grey85") +
      geom_point() +
      geom_segment(mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
      labs(title = paste("grille à la", t, "ème iteration"),
           x = "x",
           y = "y")
    return(p)
  }
}


plot_grille_2 = function(t, X, W, d, l, n){
  df = df_classes(X, W, d, n)
  p = ggplot(df, aes(x = xcoord, y = ycoord, color = Type)) +
    geom_point(data = transform(df, Classe = NULL), colour = "grey85") +
    geom_point(show.legend = FALSE) +
    facet_wrap(~Classe, ncol = l) +
    labs(title = paste("grille à la", t, "ème iteration"))
  return(p)
}
