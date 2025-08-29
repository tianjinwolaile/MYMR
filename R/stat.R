#' This is some descriptio of this function.
#' @title simple function
#' 
#' @description today,I create my first function,a very usrful function.
#' 
#' @details you can use this function to caculate x+1,then return the value of x+1.
#' 
#' @param x x is a matrix
#'
#' @return a dataframe
#' @export
#' @examples 

stat=function(x)
{
  if (all(is.na(x))) {
    stop("All elements of ", substitute(x), " have a missing value")
  }
  if(is.vector(x))
  {
    S=cbind(n=length(x),mean=mean(x),sd=sd(x),min=min(x),max=max(x))
  }
  else  
  {
    S=cbind(n=nrow(x),mean=apply(x,2,mean),sd=apply(x,2,sd),min=apply(x,2,min),max=apply(x,2,max))
  }   
  print(round(S,4))
}