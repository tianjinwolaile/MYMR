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

coef.sd=function(fm) #计算标准回归系数函数
{
  b=fm$coeff; 
  p=length(b);
  si=apply(fm$model[,2:p],2,sd); sy=sd(fm$model[,1]);
  b1=b[2:p]*(si/sy); 
  #cat("标准回归系数: ", round(b1,4));
  list(coef.sd=b1)
}

