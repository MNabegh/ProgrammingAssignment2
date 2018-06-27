## Put comments here that give an overall description of what your
## functions do

## ----------------------------------------------------------------------------------
## This function creates the global object that contains the cache for the inverse
## ----------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## ----------------------------------------------------------------------------------
## This function calls the value from the cache if exists or calculates it and save
## in the cache if it does not exist.
## ----------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
       inv <- x$getInv()
       if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setInv(inv)
       inv
}
