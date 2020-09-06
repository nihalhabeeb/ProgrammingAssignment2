## calculating inverse of a matrix can be a costly computation process.
## Caching the inverse matrix instead of computing each time saves
## computation cost.
## These functions allow to cache the inverse of a matrix and retrieve
## it when needed instead of computing each time.

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y){
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invmat <<- inv
    getinv <- function() invmat
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## Retrieves the inverse if its already calculated. Otherwise it calculates the
## inverse of the matrix object returned by the above function.

cacheSolve <- function(x, ...) {
    invmat <- x$getinv()
    if(!is.null(invmat)){
        message("getting cached matrix")
        return(invmat)
    }
    mat <- x$get()
    invmat <- solve(mat, ...)
    x$setinv(invmat)
    invmat
}
