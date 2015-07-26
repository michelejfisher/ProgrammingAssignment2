## The following two functions cache the inverse of a matrix the first time it is calculated.
## If the matrix is to be inverted again, the cache is used rather than a calculation. 
## These functions assume that the matrix is invertible.


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setSolve <- function(solve) m <<- solve
                getSolve <- function() m
                list(set = set, get = get,
                     setSolve = setSolve,
                     getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}