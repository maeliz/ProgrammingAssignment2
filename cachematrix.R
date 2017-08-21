
## Function that creates a "special" matrix with set, get, setInverse and getInverse properties

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y){
    x <<- y
    matrixInv <<-NULL
  }
  get <- function() x
  setInverse <- function(pInverse) matrixInv <<- pInverse
  getInverse <- function() matrixInv
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
  
} 


## This function retrieves a special matrix inverse. If the inverse is already computed, it returns the cached inverse

cacheSolve <- function(x,...){
  
  objInverse <- x$getInverse()
  
  if (!is.null(objInverse)){
    message("getting cached data")
    return (objInverse)
  }
  objMatrix <-x$get()
  objInverse <-solve(objMatrix)
  x$setInverse(objInverse)
  
  objInverse
}