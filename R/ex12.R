#'@export
#' exercitiul 12
#' suma si diferenta a doua variabile aleatoare continue
#' diferite folosind formula de convolutie

#' functie ce calculeaza suma
#' f - functia de densitate de probabilitate a variabilei x
#' g - ... Y
suma_convolutie <- function (f, g)
{
  function(z) (integrate (function(x) (f(x)*g(z-x)), -Inf, +Inf) $ value)
}

#' functie pentru diferenta
#' f - functia de densitate de probabilitate a variabilei x
#' g - ... Y
diferenta_convolutie <- function (f, g)
{
  function(z) (integrate (Vectorize(function(x) (f(x) * g(x-z))), -Inf, +Inf) $ value)
}

exponential <- function (x)(ifelse(x<0,0,exp(-x)))

suma_convolutie(exponential, exponential) (4)
