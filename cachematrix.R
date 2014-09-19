## File cachematrix.R
## 
## Contains functions to create a specialized matrix object that
## can compute and save its own inverse.

## Function makeCacheMatrix
##
## Input(s) x: a matrix
##
## Description: creates a object which contains a matrix
## and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # sets the matrix
  set <- function(xm) {
    x <<- xm
    m <<- NULL
  }
  
  # gets the matrix
  get <- function() x
  
  # sets the inverse matrix
  setInverse <- function(solve) m <<- solve
  
  # gets the inverse matrix
  getInverse <- function() m
  
  # list of functions to make the cache matrix
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Function cachesolve
##
## Input(s) x: a cache matrix
##
## Description: Returns the inverse of the matrix contained in a cache
##              matrix. 
cacheSolve <- function(x, ...) {
  
  # Check the cache for the inverse of matrix x
  m <- x$getInverse()
  
  # If the inverse of x exists in the cache, return that value.
  if(!is.null(m)) {
    message("Returning cached matrix")
    return(m)
  }
  
  # No inverse of x in the cache. Compute and store in cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  return(m)
  
}
