## These functions will create a special matrix object that
## caches its inverse when it's not already done so, so that
## the next time the inverse is wanted it can get it from cache.

## Creates basically a list with 4 functions, set, get, setInverse
## and getInverse.
## set: requires the square matrix as input and assigns it to x. This is the same as initializing the function.
## get: returns x, which is the square matrix
## setInverse: assign the inverse of the matrix to variable i
## getInverse: returns the value of i

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve requires an argument of type makeCacheMatrix x.
## It will return the inverse of x, using the getInverse fnction in x
## When it returns the inverse, it calls setInverse to get that inverse to x
## When it's called again, the inverse from x is no longer NULL and it returns it from cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("returning the cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
