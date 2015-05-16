## These functions will cache the inverse of a matrix so it doesn't have to be calculated
## every time you want to use it.

## makeCacheMatrix is a set of functions to:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of a matrix has been calculated, and
## if it hasn't, will calculate the inverse of a matrix and set that as the
## cached value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
