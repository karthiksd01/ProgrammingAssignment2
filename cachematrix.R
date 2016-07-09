## Functions to cache the Inverse of a Matrix
## These functions create a special object that stores a matrix and caches its inverse

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) 
                {
                        x <<- y
                        inv <<- NULL
                }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated,
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inv <- x$getInverse()   #Checking if the inverse is already calculated
        if(!is.null(inv)) 
                {
                        message("Getting cached data")
                        return(inv)
                }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
