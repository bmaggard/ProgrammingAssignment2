## Yo! Matrix inversion is a costly computation!
## If we gon' inverse a matrix once,
## We should *totally* keep it around
## so that we don' hafta do all that math again!
## Save them CPU cycles!  Save the planet!

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object
## that can cache its inverse.
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
