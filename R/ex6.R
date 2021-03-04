#'@export
#'exercitiul 6
#'functie ajutatoare pentru compunerea de functii
compose<-function(f,g) function(...) f(g(...))

#'functie ce calculeaza media si dispersia
#'g - functie introdusa de utilizator
#'d - functie de densitate
#'minim - capatul de jos al integralei
#'maxim - capatul de sus al integralei
#'paste - concatenare dupa conversia la char
medie_dispersie <- function(g, d, minim, maxim)
{
  f1 <- function (x){
    return (x* compose(g, d)(x))
  }

  medie <- integrate(f1, minim, maxim)$value

  f2 <- function (x){
    return(((x - medie) ^ 2) * compose(g, d)(x))
  }

  dispersie <- integrate(f2, minim, maxim)$value

  rezultat <- paste("Media: ", as.character(medie), ";  Dispersia: ",as.character(dispersie))
  return (rezultat)

}

#'functie care returneaza media si densitatea in functie de g(x) folosind repartitii cunoscute
#'g functie introdusa de utilizator
#'repartitie - repartitia folosita
#'a, b, n, miu, lambda, sigma - paramterii folositi in functie de repartitia aleasa
#'lognormal weibull normal chipatrat students exponential beta gamma uniform
calcul_medie_dispersie_repartitie <- function (g ,repartitie = "", a = 0, b = 0, n = 0, lambda = 0, sigma = 0, miu = 0)
{

  if (repartitie == "beta")
  {
    beto <- function (x)
    {
      if (x > 0 & x < 1)
      {
        return (1 / (beta(a, b)) * x ^ (a - 1) * (1 - x) ^ (b - 1))
      }

      return (0)
    }

    return (medie_dispersie(g, beto, 0, 1))
  }

  if (repartitie == "chipatrat"){
    chipatrat <- function (x)
    {
      if (x >= 0)
      {
        return (1 / (2 ^ (n/2) * gamma(n / 2)) * (x ^ (n / 2 - 1)) * exp(-x / 2))
      }

      return (0)
    }

    return (medie_dispersie(g, chipatrat, 0, Inf))
  }

  if (repartitie == "gamma")
  {
    gama = function (x)
    {
      if (x > 0)
      {
        return (1 / ((b ^ a) * gamma(a)) * x ^ (a - 1) * exp(-(x / b)))
      }

      return (0)
    }

    return (medie_dispersie(g, gama, 0, Inf))
  }

  if (repartitie == "exponential")
  {
    exponential <- function (x)
    {
      if (x >= 0)
        return ((1 / lambda) * exp((-x) / lambda))

      return (0)
    }

    return (medie_dispersie(g, exponential, 0, Inf))
  }
  if (repartitie == "lognormal")
  {
    lognormal <- function (x)
    {
      return((1/(sigma * x * sqrt(2 * pi))) * exp(-(((ln(x) - miu)) ^ 2) / (2 * sigma ^ 2)))
    }

    return (medie_dispersie(g, lognormal, -Inf, Inf))
  }

  if (repartitie == "normal")
  {

    normal <- function (x)
    {
      return(1 / (sigma * sqrt(2 * pi)) * exp (- (x - miu)^2/(2 * sigma ^ 2)))
    }

    return (medie_dispersie(g, normal, -Inf, Inf))
  }

  if (repartitie == "students")
  {
    students <- function (x)
    {
      return (gamma((n + 1) / 2) / (sqrt(n * pi) * gamma(n / 2)) * (1 + x^2 / n) ^ (-(n + 1) / 2))
    }

    return (medie_dispersie(g, students, 0, Inf))
  }

  if (repartitie == "uniforma")
  {
    uniforma <- function (x)
    {
      if (x >= a & x <= b)
        return (1 / (b - 1))

      return (0)
    }

    return (medie_dispersie(g, uniforma, a, b))
  }

  if (repartitie == "weibull")
  {

    weibull <- function (x){
      if(x > lambda)
      {
        return((b / n) * (((x - lambda) / n) ^ (b - 1)) * exp(-(((x - lambda) / n) ^ b)))
      }

    }

    return (medie_dispersie(g, weibull, lambda, Inf))
  }

}
