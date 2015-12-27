## THis file contains the solution to the Coursera.com R Programming Course
## second programming assignment. This file has two functions, one that creates
## a cacheble matrix object (makeCacheMatrix) and one (cacheSolve) that computes
## the inverse of the cacheble matrix returned by makeCacheMatrix

## Create a cacheable matrix object.
## @return cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    # Matrix setter
    set <- function(y) {
            x <<- y
            m <<- NULL
    }

    # Matrix getter
    get <- function() x

    # Inverse setter
    setInverse <- function(inverse) m <<- inverse

    # Inverse getter
    getInverse <- function() m

    # Return cacheable inverse matrix object
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculate inverse matrix of a cacheable matrix object
## @param x cacheable matrix object
## @return inverseMatrix the inverse of the matrix in x
cacheSolve <- function(x, ...) {

    # Try to fetch the matrix from cache
    cachedInverse <- x$getInverse()

    # If the cached matrix is set, return it
    if(!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }

    # Retrieve the matrix from x
    matrix <- x$get()

    # Caclulate the inverse
    inverseMatrix <- solve(matrix)

    # Cache the inverse
    x$setInverse(inverseMatrix)

    # Return the inverse
    inverseMatrix
}
