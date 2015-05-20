## This fuction takes a matrix and returns its inverse.

## In makeCacheMatrix, we are going to cache the result of a matrix that is not going to change
## its values. For doing that we get and set the matrix. 
## We assign the values with <<- for the function to work in different environments.
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
## The cacheSolve function will look first if the inverse is calculated, and if it is not,
## it will calculate it. Finally, will return de inverse of the matrix input to makeCacheMatrix
## function.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        my_data <- x$get()
        inv <- solve(my_data, ...)
        x$setinv(inv)
        inv
}
