#' This is some descriptio of this function.
#' @title simple function
#' 
#' @description cha zhao que shi zhi wei zhi ji que shi ge shu
#' 
#' 
#' @param data is dataframe,m is variable which
#'
#' @export
#' 
#' 

find_NA=function(data=data,m=m){
data[data == "#N/A"] <- NA
# View(data)
number <- is.na(data[,m]) # 1表示缺失,0表示不缺失
## na开头，非na结尾
if (is.na(data[,m][1]) & (!is.na(data[,m][(dim(data))[1]]))) { # na开头，非na结尾
  diff <- diff(c(0, number)) # 进行差分
  which(diff == 1)
  which(diff == -1)
  start <- which(diff == 1) # [1] 1,2339第n次出现缺失的位置
  end <- which(diff == -1) - 1 # [1] 2321 2339 缺失第n次结束的位置
  dif <- end - start + 1 # 每次缺失个数
  dif # [1] 2321    1    1
  return(list(start_which=start,end_which=end,n=dif))
  # 非na开头，na结尾
} else if (!(is.na(data[,m][1])) & is.na(data[,m][(dim(data))[1]])) {
  diff <- diff(number) # 进行差分
  which(diff == 1)
  which(diff == -1)
  start <- which(diff == 1) + 1 # [1]  425 3186 4009 8499 8567 8676第n次出现缺失的位置
  end <- c(which(diff == -1), dim(data)[1]) # [1] 1529 3186 6803 8509 8568 8699缺失第n次结束的位置
  dif <- end - start + 1 # 每次缺失个数
  dif # [1] 1104    0 2794   10    1   23
  return(list(start_which=start,end_which=end,n=dif))
  ## na开头，na结尾
} else if (is.na(data[,m][1]) & is.na(data[,m][(dim(data))[(dim(data))[1]]])) { # na开头，na结尾
  diff <- diff(c(0, number)) # 进行差分
  which(diff == 1)
  which(diff == -1)
  start <- which(diff == 1) # [1] 1 2974 3129 3186 3194 3407 3640 3822 4180 4313 4434 4569 4678 4726 4867 4973 5127 5277 5448 5999
  # [21] 6009 6645 6857 8227 8394
  # 第n次出现缺失的位置
  end <- c(which(diff == -1) - 1, dim(data)[1]) # [1] 2963 2976 3132 3186 3324 3574 3720 3952 4266 4384 4474 4619 4691 4787 4906 5032 5171 5344 5553 5999
  # [21] 6011 6714 6899 8391 8760缺失第n次结束的位置
  dif <- end - start + 1 # 每次缺失个数
  dif # [1] 2963    3    4    1  131  168   81  131   87   72   41   51   14   62   40   60   45   68  106    1
  # [21]    3   70   43  165  367
  return(list(start_which=start,end_which=end,n=dif))
  # 非na开头，非na结尾
} else if (!(is.na(data[,m][1])) & (!is.na(data[,m][(dim(data))[1]]))) { # 非na开头，非na结尾
  diff <- diff(number) # 进行差分
  which(diff == 1)
  which(diff == -1)
  start <- which(diff == 1) + 1 # [1]  425 3186 4009 8499 8567 8676第n次出现缺失的位置
  end <- which(diff == -1) # [1] 1529 3186 6803 8509 8568 8699缺失第n次结束的位置
  dif <- end - start + 1 # 每次缺失个数
  dif # [1]  1105    1 2795   11    2   24
  return(list(start_which=start,end_which=end,n=dif))
}
}
