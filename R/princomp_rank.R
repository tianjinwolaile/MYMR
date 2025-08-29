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


princomp_rank=function(data=data,m,plot=FALSE)
{
  W=as.matrix(data[[1]]^2/sum(data[[1]]^2))
  PCs=as.matrix(data$scores[,1:m])          
  PC=PCs%*%W[1:m]/sum(W[1:m])  
  #print(PC)
  ans=cbind(PCs,'PC'=PC[,1],'rank'=rank(-PC[,1]))
  #cat("\n"); print(ans)
  if(plot) 
  {
    plot(PCs);abline(h=0,v=0,lty=3)
    text(PCs,label=rownames(PCs),pos=1.1,adj=0.5,cex=0.85) 
  }
  return(ans)
} #princomp.rank(data,2,T)
