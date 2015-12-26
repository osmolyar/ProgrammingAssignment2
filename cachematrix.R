## These functions work together, taking advantage of the lexical
## scoping rules of R  to cache the potentially time-consuming operation of matrix inversion.

## The function makeCacheMatrix creates a sepacial "matrix" object that calculates its inverse.  
## This matrix is really a list containing a function to set and get the value of the 
## matrix, and set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
## set the matrix object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## get the matric object back
        get <- function() x
## set the inverse of the matrix
        setinverse <- function(solve) m <<- solve
## get the inverse of the matrix
        getinverse <- function() m
## put the setters and getters into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve calculates the inverse of the special "matrix" created 
## with makeCacheMatrix. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Try to get the cached inverse of the passed "matrix" 
        m <- x$getinverse()
## Checking if it's cached - if not, just return the cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## If the inverse was not already cached, then the above if clause would not return
## So continue to get the original matrix
        data <- x$get()
## And then get its inverse
        m <- solve(data, ...)
## Also set it in the original object
        x$setinverse(m)
## Return a matrix that is the inverse of 'x'
        m
}
