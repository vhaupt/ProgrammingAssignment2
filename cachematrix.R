## Functions that cache the inverse of matrix
## R Programming - Programming Assignment 2: Lexical Scoping

## makeCacheMatrix creates a special "matrix" object that includes the following functions:
## set the values of the matrix
## get returns the values of the matrix
## setinv sets the inverse of the matrix
## getinv returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_mat) inv <<- inverse_mat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix stored makeCacheMatrix
## object above, but first it check to see if the inverse is already cached.

## If the inverse is already cached, cacheSolve does not calculate the inverse. 
## Instead, cacheSolve returns the cached inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
