## Yo! Matrix inversion is a costly computation!
## If we gon' inverse a matrix once,
## We should *totally* keep it around
## so that we don' hafta do all that math again!
## Save them CPU cycles!  Save the planet!

## The code herein was largely copied from the 
## "Caching the Mean of a Vector" example 
## R Programming course by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## delivered by Coursera, Inc 


## This function creates a special "matrix" object
## that can cache its inverse.
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

## This function computes the inverse of the special "matrix"
## that is returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
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
