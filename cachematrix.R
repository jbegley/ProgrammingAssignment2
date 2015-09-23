## We provide 2 functions to implement caching for matrix
## inversion: makeCacheMatrix, which sets up the cache,
## and cacheSolve, which uses the cache.

## The MASS library is used to compute a pseudoinverse of non-square
## matrices.  Like other packages, this can be installed with the
## R command 'install.packages("MASS")'.
library("MASS")

## makeCacheMatrix implements a cache for the inverse of the
## matrix input as a function argument.  It returns a  4 element
## list of functions that: [1] sets a new value for the matrix (i.e.,
## values for the matrix elements), and thereby clearing the
## cached inverse; [2] gets (returns) the stored matrix;
## [3] sets the stored matrix to another value (and obliterates the
## cached inverse); and [4] gets (returns) the stored (cached) inverse
## (if available).
## Lexical scoping provides non-volatile storage.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the stored matrix.
## The argument x is the list returned by makeCacheMatrix
## when the cache was created and the matrix was first stored.
## cacheSolve returns the cached inverse if available.  If a
## cached inverse is not available, it computes the
## inverse with an appropriate function call, then
## caches and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        xdim <- dim(data)
        if(xdim[1] == xdim[2]) {
                i <- solve(data, ...)
        } else {
                i <- ginv(data, ...)
        }
        x$setinv(i)
        i
}
