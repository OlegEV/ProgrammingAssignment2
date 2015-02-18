##
## The following two functions are used to cache the inverse of a matrix rather than compute it repeatedly.
##

## 
## Creates a list of 4 member functions: setmatrix, getmatrix, setInvmatrix and getInvmatrix. 
## 

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(solve) m <<- solve
    
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

##
## Return a matrix that is the inverse of 'x', get cached version if already computed it.
##
cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    m <- solve(x$get(), ...)
    x$setInverse(m)
    m
}
