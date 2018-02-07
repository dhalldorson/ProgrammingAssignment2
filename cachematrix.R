## Put comments here that give an overall description of what your
## functions do

## This function will create a specialized "matrix" object that can cache
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function () x
     setinv <- function(solve) i <<- solve
     getinv <- function() i
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This will compute the inverse of the specialized "matrix" object returned
## by the makeCacheMatrix function. If the inverse has already been calculated
## it should just return the cached inverse.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinv()
     if (!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}
