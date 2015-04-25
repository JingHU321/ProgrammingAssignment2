# The following pair of functions can cache the inverse of a matrix. Matrix inversion
# is normally a costly computation. These functions can assign the inverse of
# a matrix to an object and the inverse can be retrieve later.

# makeCacheMatrix is a function that stores a special "matrix" that
# can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}

# cacheSolve is a function computes the inverse of the speical "matrix" returned
# by makeCachematrix function. If the inverse of the "matrix" has been caculated
# before, the cacheSolve function would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
