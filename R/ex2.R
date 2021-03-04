#'@export
#' exercitiul 2
ex2 <- function(FUN)
{
  tryCatch({variabila <- integrate(FUN, lower = -Inf, upper = Inf)$value
  if((1 - variabila) > 0.000001 || (1 - variabila) < -0.000001)
    return (FALSE)

  functia_domnul_meu <- function(x)
  {
    aux <- FUN(x)
    if(aux < 0)
      aux <- -aux
    else
      aux <- 0
    return (aux)
  }
  if(integrate(Vectorize(functia_domnul_meu), lower = -Inf, upper = Inf)$value > 0.000001)
    return (FALSE)
  return (TRUE)},
  error = function(e)
  {
    #'daca eroare inseamna ca integrala nu merge
    #' => nu e o integrala finita
    #' => ori functia nu e pozitiva ori integrala nu e finita
    #' => ori orice alta eroare poate sa dea integrala
    #'return (FALSE)
  })
  return (FALSE)
}
