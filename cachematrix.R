

makeCachemamatrix <- function (x = matrix()){
  inverse_ <- NULL
  set <-function(y){
    x<<- y
    inverse_<<- NULL
  }
  getval <- function(){x}
  set_inverse<- function(inverse){inverse_<<- inverse}
  get_inverse< function() {inverse_}
  list(set= set, get = getval, set_inverse = set_inverse, get_inverse = get_inverse)
}

cache_solve <-  function(x, ...){
  inverse_ <- x$get_inverse
  if(!is.null(inverse_)){
    message("getting the data")
    return(inverse_)
  }
  mat<- x$get()
  inv <- solve(mat, ...)
  x$set_inverse(inverse_)
  inverse_
}




