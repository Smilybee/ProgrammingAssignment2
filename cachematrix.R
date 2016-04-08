## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. The two funtions below cache the inverse of a matrix.

## Description of this function
## makeCacheMatrix creates a special matrix object which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse of the Matrix
## get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Description of this function 
## cacheSolve calculates the inverse of the special "Matrix" created with the above function. 
## It first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the
## computation. Otherwise, it calculates the Inverse of the data and sets the value in the cache via the setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}    

## Test run:
## > x = rbind(c(1, -1/9), c(-1/9, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##           [,1]       [,2]
## [1,]  1.0000000 -0.1111111
## [2,] -0.1111111  1.0000000

## No cache in the first run
## > cacheSolve(m)
##       [,1]   [,2]
## [1,] 1.0125 0.1125
## [2,] 0.1125 1.0125

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##      [,1]   [,2]
## [1,] 1.0125 0.1125
## [2,] 0.1125 1.0125
## >
