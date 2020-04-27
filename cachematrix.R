## There are two functions here makeCacheMatrix and cacheSolve
## makeCacheMatrix is a function that caches the inverted matrix.
## The makeCacheMatrix uses the R feature of Mutable state
## a static parent environment and <<- make it possible to maintain state across function call


## The makeCacheMatrix function is able to maintain state across function call.
## The inverse of a given Matrix is stored using setInverse and retreived using getInverse.
## The Original matrix is stored used setMatrix and retreived using getMatrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix , setInverse = setInverse,
       getInverse = getInverse)
  
}


## This fucntion returns the inverse of a given matrix.
## The function is optimized to return a cached inverse on subsequent calls.
## By Passing a reference to original Matrix as an additional optional argument withe attribute name "orgmatrix"
##  a comparision can be performed against the original matrix related to the inverse to check if this has changed.
# This is as per the assignment requirement "(and the matrix has not changed)"

cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  args <- list(...)
  orgMatrix <- args[["orgmatrix"]]
  print(orgMatrix)
  if(is.null(orgMatrix))
  {
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  }else{
    if(!is.null(i) && identical( x$getMatrix(), orgMatrix)) {
      message("getting cached data")
      return(i)
    }
  }  
  print("Setting up cache")
  data <- NULL
  invmatrix <- NULL
  if(!is.null(orgMatrix)){
    x$setMatrix(orgMatrix)
    invmatrix <- solve(orgMatrix, ...)
  }else{
    data <- x$getMatrix()
    invmatrix <- solve(data, ...)
  }
  x$setInverse(invmatrix)
  invmatrix
}

## This fucntion tests and proves the cache functionality works as expected
## It also shows a scenario where the original matrix has changed and the cache is refreshed as a result to return a new inverse
testCaching <- function (){
  df <- matrix( 1:4 ,  nrow =2 , ncol = 2)
  cacheMatrix <- makeCacheMatrix(df)
  dfinv <- cacheSolve(cacheMatrix)  
  print(dfinv)
  print("Subsequent calls should print gettingCached data")
  dfinv <- cacheSolve(cacheMatrix)
  # now lets change the original df matrix
  # Since the cached original matrix is different the cache is now refreshed.
  # if the orginal matrix is not changed the cache wont be refreshed and the original matrix is not reset
  df[1,1] <- 2
  dfinv <- cacheSolve(cacheMatrix , orgmatrix=df)
  print(dfinv)
  dfinv <- cacheSolve(cacheMatrix)
}