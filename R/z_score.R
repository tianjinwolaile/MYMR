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
z_score = function(B,converse=FALSE)  #converse为T或TRUE时，采用逆向指标，默认为F
{
  B=as.vector(B);
  if(converse==FALSE||converse==F||converse=="")
  {
    min_value=min(B);
    max_value=max(B);
    z_score=(B-min_value)/(max_value-min_value)*60+40;
    z_score;
  }
  else if(converse==TRUE||converse==T)
  {
    min_value=min(B);
    max_value=max(B);
    
    z_score=(max_value-B)/(max_value-min_value)*60+40;
    z_score;
  }
} #无量纲化，返回某指标的分值

