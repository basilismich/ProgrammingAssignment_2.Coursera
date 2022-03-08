## Two functions that cache the inverse of a matrix

## this function creates a "special matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  y= NULL
  ##setting the matrix
  set<-function(matrix){
    x<<-matrix
    y<<-NULL
    
  }
  ##getting the matrix
  get <- function(){
    x
  }
  ##setting the inverse
  setInverse<-function(inverse){
    y<<- inverse
  }
  ##getting the inverse
  getInverse<-function(){
    y
  }
  ##list of the steps
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## this function computes the inverse of the "special matrix" returned by the first function

cacheSolve <- function(x, ...) {
  i=x$getInverse()     
  
  
  if( !is.null(i) ) {
    message("getting the cached data")
    return(i)
    
  }
  ## Getting the matrix from our object
  getit<- x$get()
  
  ## Calculating the inverse using matrix multiplication
  i <- solve(getit) %*% data
  
  ## Setting the inverse to the object
  x$setInverse(i)
  
  ## Finishing by returning the matrix
  i
}
