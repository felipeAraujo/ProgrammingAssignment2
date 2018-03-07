## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    get <- function() x
    setSolved <- function(solved) s <<- solved
    getSolved <- function() s
    list(
        get = get,
        setSolved = setSolved,
        getSolved = getSolved
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolved()
    if (!is.null(s)) {
        message('getting cached data')
        return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setSolved(s)
    s
}
