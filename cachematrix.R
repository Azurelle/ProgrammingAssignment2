# cachematrix.R
# 
# This script contains two functions:
# 1. makeCacheMatrix()
#    input parameter:  invertible matrix - class = matrix
#    return value: list with functions set(), get(), setinverse(), getinverse()
# The returned list contains functions to 
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse - should NOT be called directly
#    get the value of the inverse
#
# 2. cacheSolve()
#    input:  output from makeCacheMatrix() - class = list
#    return: matrix inverse
# The function checks to see if the inverse has been calculated previously, and
# if so, retrieves it from the cache and returns it.  If not, it calculates 
# the inverse using "solve()", sets the result in the cache, and returns the 
# result.


# Creates a special "matrix" object that can cache its inverse. 
# Input matrix x must be invertible.  
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        # Set the value of the matrix
        set <- function(y) {
                x <<- y  
                i <<- NULL 
        }
        
        # Get the value of the matrix
        get <- function() x 
    
        # Set the value of the inverse - do NOT call this directly
        setinverse <- function(inverse) i <<- inverse
        
        # Get the value of the inverse
        getinverse <- function() i 
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix().
# If the inverse has already been calculated for the given matrix, the inverse
# from the cache is returned
cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        # Check to see if inverse has already been calculated
        # If so, get inverse from the cache and exit function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Else, calculate the inverse of the data
        # Set the value of the inverse in the cache via the setinverse function from above
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}










