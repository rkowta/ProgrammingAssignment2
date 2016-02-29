## makeCacheMatrix caches both the input matrix, and its inverse, so that the 
## inverse of a matrix is calculated only once.
## cacheSolve returns the inverse of matrix.

## makeCacheMatrix accpets a matrix as input and returns a special "matrix" object
## which stores both the matrix and its inverse. It does this by using a closure
## function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function(y) x
    setInverse <- function(y) inv <<- y
    getInverse <- function(y) inv
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function accepts a matrix as input and returns the inverse of the matrix.
## If the input to cacheSolve function is the special matrix created by makeCacheMatrix
## function, then the inverse is calculated only one time on the first call.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if ("getInverse" %in% names(x)){
        inv <- x$getInverse()
    }
    else{
        return(solve(x))
    }
 
    if (!is.null(inv)){
        message("getting cached data for inverse")
        return(inv)
    }
    mat1 <- x$getMatrix()
    inv <- solve(mat1)
    x$setInvers(inv)
    inv
}
