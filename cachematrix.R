## Caching matrix
makeCacheMatrix <- function(x = matrix()) {
    cachematrix <- NULL
    
    set <- function( matrix ) {
        m <<- matrix
        cachematrix <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(inverse) {
        cachematrix <<- inverse
    }
    
    getInverse <- function() {
        cachematrix
    }
    
    # Return the list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse if not present in the Caching matrix
cacheSolve <- function(x, ...) {
    inversematrix <- x$getInverse()
    
    if( !is.null(inversematrix) ) {
        message("getting cached data")
        return(inversematrix)
    }
    
    # Otherwise
    data <- x$get()
    
    # Calculate the inverse using matrix multiplication
    inversematrix <- solve(data)
    
    # Set the inverse to the object
    x$setInverse(inversematrix)
    
    inversematrix
}
