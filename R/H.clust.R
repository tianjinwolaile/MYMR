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
H.clust=function(X,d="euc",m="comp",proc=FALSE,plot=TRUE)
{
  D=dist(X,d)
  hc <- hclust(D,m)            
  #if(proc){ cat("\n cluster procdure: \n"); print(cbind(hc$merge,hc$height)) }
  PROC=cbind(merge=hc$merge,height=hc$height)
  if(proc) print(PROC)
  if(plot) plot(hc,ylab=d,main=m)    
  #plot(hc,hang=hang,xlab="",ylab="",main="")    
  #hc1=as.dendrogram(hc)
  #plot(hc1,xlab="G",ylab="D",horiz=TRUE) 
  #list(D=D,hc=hc,proc=proc)
  return(hc)
} #C=H.clust(X)