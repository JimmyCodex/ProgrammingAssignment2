## THis file contains the solution to the Coursera.com R Programming Course
## second programming assignment. This file has two functions, one that creates
## a cacheble matrix object (makeCacheMatrix) and one (cacheSolve) that computes
## the inverse of the cacheble matrix returned by makeCacheMatrix

## Create a cacheable matrix object.
## @return cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }

    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculate inverse matrix of a cacheable matrix object
## @param x matrix object
## @return cacheable matrix inverse
cacheSolve <- function(x, ...) {

    # Return a matrix that is the inverse of 'x'
    cachedInverse <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(cachedInverse)
    }

    matrix <- x$get()
    inverseMatrix <- solve(matrix)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
