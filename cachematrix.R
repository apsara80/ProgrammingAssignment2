## Matrix inversion is a notoriously cumbersome computation and therefore it can save
## time and memory space by caching the inverse of a matrix rather than computing it
## each time repeatedly. The following pair of functions cache the inverse of 
## matrix, using lexical scoping in R.

## This function creates a special "matrix" object that can cache its inverse.
## It 'sets' and 'gets' a matrix object to be passed down to the next fuction, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list (set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse matrix has already been calculated,
## then this function retrieves the inverse from the cache. 
## If the inverse hasn't been calculated, this function calculates it using 'solve()'
## and returns the result.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        myMatrix <- x$get()
        inv <- solve (myMatrix, ...)
        x$setinv(inv)
        inv
}

