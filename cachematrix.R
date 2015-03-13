## makeCacheMatrix create a special matrix that provides a list of functions
##  1, set the value of the matrix
##  2, get the value of the matrix
##  3, set the value of inversed matrix
##  4, get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Return a list of functions from the input matrix
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse    
}


# test case
# > inverse1 <- makeCacheMatrix( matrix(c(1, -1, 2, 1), nrow = 2, ncol = 2, byrow = TRUE) )
# > cacheSolve( inverse1 )
# [,1]      [,2]
# [1,]  0.3333333 0.3333333
# [2,] -0.6666667 0.3333333
# > cacheSolve( inverse1 )
# getting cached inverse matrix
# [,1]      [,2]
# [1,]  0.3333333 0.3333333
# [2,] -0.6666667 0.3333333
