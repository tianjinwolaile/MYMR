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
factanal.rank=function(Fac,plot=FALSE)
{
  Fs=Fac$scores          
  W=apply(Fac$loadings^2,2,sum)
  Wi=W/sum(W);
  F=Fs%*%Wi  
  #cat("\n"); print(cbind('F'=F[,1],'rank'=rank(-F[,1])))
  Ri=data.frame('F'=F,'rank'=rank(-F))
  if(plot)
  {
    plot(Fs);abline(h=0,v=0,lty=3)
    text(Fs,label=rownames(Fs),pos=1.1,adj=0.5,cex=0.85) 
  }
  #common=apply(Fac$loadings^2,1,sum);
  list(Fs=Fs,Ri=Ri)
}#Fac=factanal(X,2,rot="varimax",scores="regression")
