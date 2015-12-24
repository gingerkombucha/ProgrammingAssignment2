## The following functions are used to compute the inverse of a matrix
## making use of the concept of lexical scoping, which is specifically used
## by the R language.

## The first function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## The second function computes the inverse of the special matrix, but 
## before that it checks the cache to see if it is already present 
## using lexical scoping, and if it finds the computed inverse, it retrives
## it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
