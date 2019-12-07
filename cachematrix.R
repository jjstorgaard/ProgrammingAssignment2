## These functions work together by 'makeCacheMatrix' creating an object which
## stores the original matrix as well as the inverted matrix once that has been
## calculated. 'cacheSolve' either computes the inverted matrix or retrieves the
## already computed one from the object created with 'makeCacheMatrix'.

## Outputs a list of functions to set and get the original matrix and the
## inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Uses the list created by the first function to either compute an inverted
## matrix or retrieve it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}