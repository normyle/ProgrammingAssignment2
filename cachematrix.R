## These functions are designed to create a matrix and cache its inverse

## This function sets and gets the matrix and sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calcuates the matrix inverse, but first checks to see
## if it already has been solved

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
## Test functions to see if they work
mymatrix <- matrix(data = c(3,4,1,2), nrow = 2, ncol = 2)
test <- makeCacheMatrix(mymatrix)
cacheSolve(test)