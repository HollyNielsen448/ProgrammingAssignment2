## Put comments here that give an overall description of what your
## functions do:
#The following functions cache the inverse of a matrix instead of repeatedly 
#computing the inverse of a matrix,which is a costly computation, these functions create 
#a special object which stores the matrix and caches its inverse

## Write a short comment describing this function:
#The function makeCacheMatrix creates a special object (list containing a 
#matrix and functions) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(Matrixinverse) inv <<- Matrixinverse
  getinverse <- function()inv
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

## Write a short comment describing this function:
#The function cacheSolve computes the inverse of the special matrix created by 
#makeCacheMatrix. It checks if an inverse is cached already and returns the 
#value if there is. It otherwise computes the inverse, caches it, and 
#returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <-x$get()
  inv <- solve(matrix,...)
  x$setinverse(inv)
  inv
  }
