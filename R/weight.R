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
#' 
weight = function(B) #B为构造的判断矩阵，按行以向量的形式提供参数
{
  A=matrix(B,nrow=sqrt(length(B)),ncol=sqrt(length(B)),byrow=TRUE)
  #A=t(B);  
  n=ncol(A);
  mul_collect=c(1:n);
  for(i in 1:n)
    mul_collect[i]=prod(A[i,])
  weight=mul_collect^(1/n);
  weight_one=weight/sum(weight);
  round(weight_one,4)
}#返回权重向量