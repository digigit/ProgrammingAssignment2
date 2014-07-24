## The aim of these functions is to avoid recalculating the inverse of a square
## matrix by caching the inversion results, using makeCacheMatrix(), and 
## reusing the result, if available, when required via cacheSolve(). If the 
## inverse has not been previously calculated, it is obtained using solve() and
## stored in the cache.

## makeCacheMatrix() creates a vector of functions. 
## These functions are:
## the getter and setter function pair for the matrix, and,
## the getter and setter function pair for the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL  # init the store for the inverse
    # define the matrix setter
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    # and now the matrix getter
    get <- function() x
    # setter for the inverse
    set.inverse <- function(inv) inverse.matrix <<- inv
    # and the getter
    get.inverse <- function() inverse.matrix
    # store the elements in a list
    list(set = set, get = get, set.inverse = set.inverse, 
         get.inverse = get.inverse)    
}


## cacheSolve() checks whether the inverse matrix of x has been previously
## calculated. If so, it grabs the inverse from the cache and returns it.
## If the cache is empty, it calculates the inverse of x and stores it.
cacheSolve <- function(x, ...) {
    # Check whether the inverse already lives in the cache
    inv <- x$get.inverse()
    # If the inverse exists, notify and return it
    if(!is.null(inv)) {
        message("getting inverse matrix from the cache")
        return(inv)
    }
    ## Inverse matrix not found in the cache, need to solve() it
    # Get the original matrix
    the.matrix <- x$get()
    # Calculate the inverse
    inv <- solve(the.matrix)
    # Save it to the cache
    x$set.inverse(inv)
    # Return the inverse matrix
    inv
}
