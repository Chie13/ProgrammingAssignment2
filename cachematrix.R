## Computing the inverse of a square matrix
## after the inverse has been computed,it will be in the cache and 
## and return it, and never compute it again.

## this function named "makeCacheMatrix" will create a matrix that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) z <<-inverse
    getinverse <- function() z
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this function named "cacheSolve" will computes the inverse of the matrix created above.
## after the inverse was computed, this function will collects it, and if not it will continue to compute it until the inverse is computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    z <- x$getinverse()
    if (!is.null(z)) {
        message("getting cached inverse matrix")
        return(z)
    } else {
        z <- solve(x$get())
        x$setinverse(z)
        return(z)
    }
}
