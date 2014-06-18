## Below are two functions that are used to create a special object 
## which stores a matrix and can cache its inverse


## This function creates a special matrix which is realy a list of four 
## functions to get and set the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix". If the inverse 
## has already been calculated, then it should be retrieved from the cache.

cacheSolve <- function(x, ...) 
{
    inverse <- x$getinv()
    if (!is.null(inverse))
    {
        message("getting inverse from cache")
        return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setinv(inverse)
    inverse
}
