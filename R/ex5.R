#'@export
#' exercitiul 5
momentOrdinK <- function(FUN, k)
{
  if(!ex2(FUN))
    return("refuz")
  functia_domnul_meu <- function(x)
  {
    return ((x^k)*FUN(x))
  }

  tryCatch({
    return(integrate(Vectorize(functia_domnul_meu), lower = -Inf, upper = Inf)$value)
  },
  error = function(e)
  {
    #'daca eroare inseamna ca integrala nu merge
    #' => nu e o integrala finita
    #' => ori functia nu e pozitiva ori integrala nu e finita
    #' => ori orice alta eroare poate sa dea integrala
    #'return (FALSE)
  })
  return("refuz")
}
momentCentratK <- function (FUN,k){
  if(!ex2(FUN))
    return("refuz")
  med <- momentOrdinK(FUN,1)
  if(typeof(med) != "double")
    return("refuz")

  functia_domnul_meu <- function(x)
  {
    return ((x-med)^k*FUN(x))
  }

  return(integrate(Vectorize(functia_domnul_meu), lower = -Inf, upper = Inf)$value)
}
ex5 <- function(FUN)
{
  return(c(
    momentOrdinK(FUN,1),
    momentCentratK(FUN,2),
    momentOrdinK(FUN,1),
    momentOrdinK(FUN,2),
    momentOrdinK(FUN,3),
    momentOrdinK(FUN,4),
    momentCentratK(FUN,1),
    momentCentratK(FUN,2),
    momentCentratK(FUN,3),
    momentCentratK(FUN,4)
  ))
}
