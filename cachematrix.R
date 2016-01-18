## For caching the inverse of a matrix

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL      # clear cache if makeCacheMatrix is invoked
    set <- function (y) {
        x <<- y        # assign to x in the parent environment
        invrs <<- NULL  # clear cache if the matrix is reset
    }
    get <- function() x
    setinvrs <- function(inverse) invrs <<- inverse
    getinvrs <- function() invrs
    list (set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
}


## The cacheSolve function computes the inverse of the special object
## created by makeCacheMatrix if the inverse has not been calculated.
## Otherwise, the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
            message("getting cached data")
            return(invrs)
        }
        matrix <- x$get()   
        invrs <- solve(matrix, ...)
        x$setinvrs(invrs)     # Store inverse matrix in cache
        invrs
}

