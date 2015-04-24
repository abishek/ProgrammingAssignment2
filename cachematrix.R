## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 
## Following are a pair of functions that cache the inverse of a matrix.
## These are expected to work only for square invertible matrix.

## makeCacheMatrix creates a special matrix that stores/caches its inverse.
##                 The special matrix is a list that has functions to
##                 1) set the matrix
##                 2) set the inverse of the matrix
##                 3) get the matrix
##                 4) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inv to NULL
    inv <- NULL
    ## setter functions
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    setinv <- function(i) inv <<- i
    ## getter functions
    get <- function() x
    getinv <- function() inv
    
    ## the special matrix as a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the special matrix 'x'.
##            The function first checks to see if 'x' already stores its 
##            inverse. If yes, it returns the existing inverse. If not, this
##            function computes the inverse and stores it in 'x' while returning
##            the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        ## cached inverse present. return that.
        message("returning cached inverse")
        return(inv)
    }
    ## no cached inverse. get the matrix and solve for its inverse
    m <- x$get()
    inv <- solve(m)
    ## cache the computed inverse for future reference.
    x$setinv(inv)
    ## return the inverse
    inv
}
