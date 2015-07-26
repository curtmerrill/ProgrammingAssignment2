## makeCacheMatrix and cacheSolve are a pair of functions
## for solving a matrix and caching the result.

## makeCacheMatrix creates a special matrix object
## that stores a copy of its inverse (once solved)
## for faster retreival

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL

  ## set the value of the matrix to be stored
  ## initialize the inverse to NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }

  ## retreive the original matrix
  get <- function() x
  
  ## set value of the inverse
  setInverse <- function(inv) matrixInverse <<- inv
  
  ## get value of inverse
  getInverse <- function() matrixInverse
  
  ## return the new list, which has the functions built in
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve tries to return the cached value of the solved
## matrix, if it is available. If not, it will solve the matrix
## and store the result for retrieval in the future

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("Retrieving cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data,...)
  x$setInverse(matrixInverse)
  matrix(matrixInverse)
}
