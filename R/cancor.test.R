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
cancor.test=function(x,y,plot=FALSE)
{
  x=scale(x); y=scale(y);
  n=nrow(x);p=ncol(x);q=ncol(y);
  ca=cancor(x,y)
  cat("\n"); print(ca);
  #cancor.test(ca$cor,n,p,q)
  r=ca$cor 
  m<-length(r); Q<-rep(0, m); P=rep(0,m); lambda <- 1
  for (k in m:1){
    lambda<-lambda*(1-r[k]^2); 
    Q[k]<- -log(lambda)  
  }
  s<-0; i<-m 
  for (k in 1:m){
    Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
    P[k]<-1-pchisq(Q[k], (p-k+1)*(q-k+1))
  }
  cat("cancor test: \n"); print(cbind(r,Q,P))
  if(plot){
    u=as.matrix(x)%*%ca$xcoef
    v=as.matrix(y)%*%ca$ycoef
    plot(u[,1],v[,1],xlab='u1',ylab='v1')
    abline(lm(u[,1]~v[,1]))
  }
}
#cancor.test(XY[,1:3],XY[,4:6],plot=TRUE)