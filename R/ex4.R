#'@export
#' exercitiul 4
#'afisare functia de densitatea a  probabilitatii pentru variabile aleatoare
#'s - cat de apropiate sunt punctele
#'trebuie
plot_densitate <- function(f, minim, maxim) {
  s <- seq(minim, maxim, 0.01)
  plot (s, f (s), col="green", main = 'Densitatea de Probabilitate')
}


#'afisare funtie de repartitie
afisare_repartitie <- function(f, minim, maxim) {
  s <- seq(minim, maxim, 0.025)
  repr <- c()
  for (i in s) {
    repr = append (repr, integrate(f, -Inf, i) $ value)
  }
  plot (s, repr, col = 'green', main = 'Functia de Repartitie')
}
