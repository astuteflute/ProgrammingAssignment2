## Cache for a Vector and Its Inversion
## Adapted from `Caching the Mean of a Vector - rdpeng`

## Uses R scoping to create an environment that caches a matrix and its 
## inversion. The inverted matrix is initially NULL indicating it has not been 
## calculated. After the inversion for the matrix has been calculated it is 
## stored and reused from the cache avoiding the overhead of recalculation. When
## the matrix in the cache is changed, the inverted matrix is reset to NULL so
## it will cause calculation on the next request for the invert.

## Usage:
##
## mc <- makeCacheMatrix(m1)  run 1st, sets up a cache with an initial matrix 
## 
## The output from makeCacheMatrix is the argument for cacheSolve
## 
## im <- cacheSolve(mc)  returns the inverse 
## 
## mc$setmatrix(m2)  change the matrix in the cache


## makeCacheMatrix: Set up a cache for a matrix and its inversion and store the
## initial matrix in the cache. Returns a list of named functions

makeCacheMatrix <- function(cached_matrix = matrix()) {
    cached_invert <- NULL
    
    setmatrix <- function(new_matrix) {
        cached_matrix <<- new_matrix
        cached_invert <<- NULL
    }
    getmatrix <- function() {
        cached_matrix
    }
    setinvert <- function(invert) {
        cached_invert <<- invert
    }
    getinvert <- function() {
        cached_invert
    }
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinvert = setinvert,
         getinvert = getinvert)
}


## cacheSolve: Returns the inverse of the matrix in the cached environment
## associated to the `mc` argument. If the matrix in the cache is new or
## changed, the inverse is calculated. Otherwise the cached version of the
## inverse is used.

cacheSolve <- function(mc, ...) {

    inverted <- mc$getinvert()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- mc$getmatrix()
    inverted <- solve(data, ...)
    mc$setinvert(inverted)
    inverted
}
