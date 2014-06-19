## These functions allow us to calculate the inverse of
## a square, invertible matrix. Notably, however, it uses
## R's scoping rules to cache the value of the inverse.
## This way, it will always to check if there is an existing,
## cached inverse value before calculating an inverse, hence
## speeding up computation times for repeated calculations
## of the inverse of large matrices.

## Before finding the inverse of the matrix, we need to cast
## it into the write form. This function takes the matrix we
## want to invert, and provides methods for getting and setting
## the data and inverse values, contained within the new 'cache
## matrix'

makeCacheMatrix <- function(x = matrix()) {
  ## When we create a new 'cacheMatrix' object, it does not have
  ## an existing inverse value, so the inverse is NULL
  inv <- NULL
  
  ## Create setter and getter functions so we can access the
  ## cacheMatrix object after we make it
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## Create setter and getter functions to access the cached
  ## inverse matrices
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks to see if there is a cached inverse
## available for the given matrix, and if there is not, it
## calculates it using the built-in R function solve().

cacheSolve <- function(x, ...) {
  ## First check to see if a cached inverse exists
  inv <- x$getinverse()
  
  ## If it does, get it from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    ## There is no need to continue to calculate the inverse
    ## so we can return a value and exit the function
    return(inv)
  }
  
  ## If not, we need to calculate it:
  ## First get the data from the 'cacheMatrix' object
  data <- x$get()
  ## Calculate the inverse using the R function solve()
  inv <- solve(data, ...)
  ## Set the inverse in the cacheMatrix object
  x$setinverse(inv)
  ## Finally, return the inverse
  inv

}
