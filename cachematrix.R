## makeCacheMatrix creates an object for manipulating objects in cache
## cacheSolve calculates the inverse of a matrix and records it in cache

## This function defines four functions for storing the matrix and its inverse 
## to cache
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  # This function will retrieve x from the parent environment
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Calculates the inverse for a makeCacheMatrix object
## Requires an object of class makeCacheMatrix as input
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
