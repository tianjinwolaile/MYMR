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


reg.plot=function(fm)
{
  p=ncol(fm$model);
  if(p==2){ plot(fm$model[,2],fm$model[,1],
                 xlab=names(fm$model[2]),ylab=names(fm$model[1])); abline(fm,col='red');} 
  else{ plot(rownames(fm$model),fm$model[,1],type='p',xlab='i',ylab='.y,-y^');
    lines(rownames(fm$model),fm$fit); }
}