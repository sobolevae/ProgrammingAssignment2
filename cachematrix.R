## This pair of functions lets: 
## - get a list of four functions for set and get a matix and a result 
##     of it's inversion; this function allows to cache the result 
##     of inversion;
## - get the result of inversion of the special "matrix" obtained by 
##     the first function using the cashed result if possible.

## This function recieves an invertable matrix 
## and returnes a list of four functions that allow 
## to set and get a matrix and a result of it's inversion

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv_x <- function(inv_y) inv_x <<- inv_y
    getinv_x <- function() inv_x
    list(set = set, get = get, setinv_x = setinv_x, getinv_x = getinv_x)
}


## This function recieves a special "matrix" that shoud be 
## a result of "makeCacheMatrix" function 
## and returns the result of matrix inversion;
## the cached result of previous matrix inversion 
## will be used if possible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinv_x()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data_x <- x$get()
    inv_x <- solve(data_x, ...)
    x$setinv_x(inv_x)
    inv_x
}
