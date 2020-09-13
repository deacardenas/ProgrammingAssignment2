## Calculate and cache value of the inverse of a matrix

## Creates a list with functions to get and set 
#both the matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    getinv <- function() inv_x
    setinv <- function(inv) inv_x <<- inv
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}

## Computes the inverse of the "matrix" returned by  makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if (!is.null(inv_x)){
        message("getting cached data")
        return(inv_x)
    }
    mat <- x$get()
    inv_x <- solve(mat, ...)
    x$setinv(inv_x)
    inv_x
}