## This file contains R function is able to cache potentially time-consuming computations

## This function creates a special "matrix" object
## The function, "makeCacheMatrix" creates a special "matrix", that can cache its inverse,
## which is really a list containing a function to
## set - the value of the matrix
## get - the value of the matrix.
makeCacheMatrix <- function( x = matrix() ) {
    i <- NULL

    set <- function(y) x <<- y
    get <- function( ) x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## This function computes the inverse of the special "matrix"
cacheSolve <- function( x, ... ) {
    inverse <- x$getinverse()

    if ( ! is.null(inverse) ) {
        message('Getting cached data')
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
