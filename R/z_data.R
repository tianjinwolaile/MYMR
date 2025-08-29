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

z_data = function(data,converse=FALSE)
{
  n=ncol(data);
  m=nrow(data);
  score_array=array(1:(m*n),c(m,n));
  for(i in 1:n)
  {
    score_array[,i]=z_score(data[,i],converse);
  }
  SCORE=as.matrix(score_array);
  dimnames(SCORE)[1]=dimnames(data)[1];
  dimnames(SCORE)[2]=dimnames(data)[2];
  round(SCORE,4)
}#输出标准化分数，data为读取的数据
