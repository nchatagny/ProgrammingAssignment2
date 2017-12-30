
## Coursera Data Science Specialisation - R Programming Assignment 2

## These functions create a special matrix object, which allow the computed inverse 
## to be cached so as to not have to compute it repeatedly.

## makeCacheMatrix is the function which creates the special matrix object, which can 
## then be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the special matrix object computed by makeCacheMatrix
## and checks whether its inverse has already been computed. If it has already been
## computed, the function retrieves the inverse from the cache, otherwise it computes
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
