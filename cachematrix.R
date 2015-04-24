## These functions cache the inverse of a matrix
## Assumptions: the matrix supplied is always invertible

## The function makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) Inv <<- solve
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
                if(!is.null(Inv)) { # if Inv exists then cache Inv
                        message("getting cached data")
                        return(Inv)
                        }
        # else get inverse of x
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)#sets the inverse of x in the cache via the `setinverse` function
        Inv
}
