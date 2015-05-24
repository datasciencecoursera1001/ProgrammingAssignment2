## This file contains  a pair of functions that cache the inverse of a 
## matrix.  This is for Programming Assignment 2 of Coursera's 
## "R Programming".  
## Note: these comments borrow heavily from the assignment description.
## Note: the solution is not very different from the example.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(inverse) inv <<- inverse
    getSolve <- function() inv
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix (above). If the inverse has already been 
## calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
}
