#' @export
#' exercitiul 1

ex1 <- function(FUN)
{
  #'test
  functia_mea <- function(x)
  {
    aux <- FUN(x)
    if(aux < 0)
      aux <- -aux
    else
      aux <- 0
    return (aux)
  }
  tryCatch(
    {
      variabila <- integrate(Vectorize(functia_mea), lower = -Inf, upper = Inf)$value
      #' daca noua integrala > 0, atunci functia nu e pozitiva
      if(variabila > 0.000001)
        return (FALSE)

      variabila <- integrate(FUN, lower = -Inf, upper = Inf)$value
      #'daca integrala nu e finita nu poate fi normalizata
      if( ! (variabila < Inf && variabila > -Inf))
        return (FALSE)
      return (1/variabila)
    },
    error = function(e)
    {
      #'daca eroare inseamna ca integrala nu merge
      #' => nu e o integrala finita
      #' => ori functia nu e pozitiva ori integrala nu e finita
      #' => ori orice alta eroare poate sa dea integrala
      #'return (FALSE)
    }
  )
  return(FALSE)
}
