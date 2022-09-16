#' Euclidean function to calculate the Greatest Common Divisor(GCD)
#' @param a the first numeric scalar/integer argument
#' @param b the second numeric scalar/integer argument
#' @return Calculated Greatest Common Divisor(GCD) from arguments(a and b)
#' @references
#' \url{https://en.wikipedia.org/wiki/Euclidean}
#' @export

#euclidean Function

euclidean <- function (a,b){
  while(b>0){
    t<-b
    b<- a%%b
    a <-t
  }

  return (a)
}

