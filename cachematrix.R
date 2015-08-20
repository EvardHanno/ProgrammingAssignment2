## Functions to calculate inverse of a matrix and cache the result so the 
## inverse does not have to be calculated again if already done once


## Function caches the matrix (set) and returns it (get)
## Function caches the calculated inverse of matrix (setinv) and returns it (getinv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function returns the inverse of matrix (the variable to be defined for cacheSolve
## is makeCacheMatrix(m) where m is the matrix). If the inverse value has already been found
## the information is taken from cache. If the inverse has not been found yet the inverse is calculated
## and returned to setinv() in makeCacheMatrix function to be saved in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
