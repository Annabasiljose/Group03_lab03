#' Euclidean function to calculate the Greatest Common Divisor(GCD)
#' @param a the first numeric scalar/integer argument
#' @param b the second numeric scalar/integer argument
#' @export

#euclidean Function
euclidean <- function(a,b){
  gcd_result <- 0
  counter_limit <-0
  counter <-1
  if(!is.numeric(a)|| !is.numeric(b)){
    stop("Input values have to be Numeric")
  }
  #Find the last number for our loop

  if(abs(a)>abs(b)){
    counter_limit <-abs(b)
  }else{
    counter_limit <-abs(a)
  }
  #Find the Greatest Common Divisor
  while(counter<=counter_limit){

    if((abs(a) %% counter == 0) && (abs(b) %% counter == 0)) {
      gcd_result = counter
    }
    counter <-counter+1
  }
  #Return Greatest Common Divisor
  return(gcd_result)
}
