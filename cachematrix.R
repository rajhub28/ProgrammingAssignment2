## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This function conatructs a Cache Matrix object
## with the ability to save the inverse of the
## matrix and recalled when needed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##
## This function computes the inverse of a matrix, assuming it has one
## It first checks if the inverse is already available and if so, it
## simply returns the inverse rather than calculate it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
