#' This is some descriptio of this function.
#' @title simple function
#' 
#' @description 构建空间权重矩阵
#' 
#' @details #说明，path为.gal的路径,k表示我们矩阵的维数，word表示我们判断相邻的值，比如从（1,2,3,4），为了求布尔值，相加就得到我们需要的空间权重矩阵
#' 
#' @param #说明，path为.gal的路径,k表示我们矩阵的维数，word表示我们判断相邻的值，比如从（1,2,3,4），为了求布尔值，相加就得到我们需要的空间权重矩阵
#'
#' @return a dataframe
#' @export
#' @examples 
#'word=read.csv("D:\\迅雷下载\\中华人民共和国 (1)\\云南.csv")[,1]
#'r=weight_(path="D:\\迅雷下载\\中华人民共和国 (1)\\云南省.gal",k=31,word=word)
#说明，path为.gal的路径,k表示我们矩阵的维数，word表示我们判断相邻的值，比如从（1,2,3,4），为了求布尔值，相加就得到我们需要的空间权重矩阵
weight_=function(path=path,k=k,word=word){
  library(spdep)
  library(stringr)
  #说明，path为.gal的路径,k表示我们矩阵的维数，word表示我们判断相邻的值，比如从（1,2,3,4），为了求布尔值，相加就得到我们需要的空间权重矩阵
  #word=read.csv("D:\\迅雷下载\\中华人民共和国 (1)\\云南.csv")[,1]
  #r=weight_(path="D:\\迅雷下载\\中华人民共和国 (1)\\云南省.gal",k=31,word=word)
  d=data.frame(read.geoda(path))
  s=d[[1]]#提取我们的结果，结果有很多行
  matrix_weight=data.frame(matrix(0,k,k))#我们最终的结果矩阵
  for(i in 1:(round(length(s)/2)-1)){
    dd=str_split(s[2*i]," ")#把每行的值分割开
    s1=dd[[1]]#得到每行的分开结果
    n=length(s1)#计算每个相邻总数
    result=rep(0,31)#记录布尔值
    for (j in 1:n){
      # result=result+as.numeric(as.numeric(s1[j])==read.csv("D:/统计建模/2014.csv")[,1])
      result=result+as.numeric(as.numeric(s1[j])==word)#累加获得最终的矩阵
      
    }
    matrix_weight[,i]=result#获取矩阵
  }
  return(list("matrix_weight"=matrix_weight))
}
# library(sf)
# fname <- system.file("shape/nc.shp", package="sf")
# fname
# nc <- st_read(fname)
# ss=st_read("D:\\新建文件夹\\中华人民共和国\\中华人民共和国.shp")
# ss