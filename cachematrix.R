## This set of functions checkes if inversion of inputted matrix exists in the cache
## If it exists, it returnes cached inverted matrix
## If it doesn't exist, cacheSolve calculates inversion of this matrix and then
## saves it into cache and returns it as output.

## makeCacheMatrix creates list of functions:
## set - assigns to 'x' value of inputed matrix and empties cache (value of variable 'inv')
## get - outputs 'x'
## setinverse - assigns to 'inv' value of 'inverse'
## getinverse - outputs 'inv'

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() {
                        x
                }
                setinverse <- function(inverse) {
                        inv <<- inverse
                }
                getinverse <- function() {
                        inv
                }
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## cacheSolve takes from cache value of inverted matrix (inv) and then
## checkes if 'inv' is not empty. If it's true, function return value of 'inv'(which is
## our chached data), else (if 'inv' is empty) it takes value of inputed matrix,
## calculates its inversion, assigns result to cache ('inv') and outputs result.

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
