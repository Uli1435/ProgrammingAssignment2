## We will create two functions that help us with a Matrix inversion. Because 
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # It will hold the cache value or if there is nothing
    # cached, there will be NULL
    m <- NULL
    
    # Create a function to store a matrix
    matrixSet <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # We return the stored matrix
    matrixGet <- function() x
    
    # We save the value
    inverse_m <- function(inv){
        m <<- inv
    }
    
    # We get the saved value
    inverseGet <- function() m
    
    # Return the list.
    list(matrixSet = matrixSet, matrixGet = matrixGet, inverse_m = inverse_m, inverseGet = inverseGet)
}

## This function computes the inverse of the special matrix returned by the 
## function makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then 'cacheSolve' should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Get the cached value
    inverse <- x$InverseGet()
    
    # If there is a cached value then return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If value don't exists, get the matrix, calculate the inverse and store it
    data <- x$matrixGet()
    inverse <- inv(data)
    x$inverseGet(inverse)
    
    # Return the inverse
    inverse
}
