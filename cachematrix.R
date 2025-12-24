## This file contains functions that demonstrate lexical scoping in R by
## creating a special matrix object that can cache the inverse of a matrix.
### makeCacheMatrix creates a special "matrix" object that stores a matrix
## and caches its inverse using lexical scoping.



## makeCacheMatrix creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

## cacheSolve computes the inverse of the special matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
