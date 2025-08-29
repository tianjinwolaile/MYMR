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
plot.text=function(X,h=0,v=0)
{
  plot(X);abline(h=h,v=v,lty=3)
  text(X,label=rownames(X),pos=1.1,adj=0.5,cex=0.85) 
}