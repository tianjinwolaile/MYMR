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

S_rank=function(data,Wi) #计算最终的加权评分
{
  wight_matrix=matrix(Wi,ncol=1,byrow=FALSE);
  score_matrix=as.matrix(data);
  Si=score_matrix%*%wight_matrix;
  print(data.frame(Si=Si,ri=rank(-Si)))
  #list(Si=Si,ri=rank(-Si))
  list(Si=Si)
}#计算加权分值

