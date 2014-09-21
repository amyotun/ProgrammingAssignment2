# Purpose:              To cache Matrix (invertable matrix)
# Function argument:    x is the matrix
# Return:               list consisting of 4 matrices
# Assumption:           Input matrix is invertable

makeCacheMatrix <- function(x = matrix()) {
        # Initialize variable
        m <- NULL
        
        # Set the matrix by y(matrix)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Get the matrix information
        get <- function() {
                x
        }
        
        # Set the matrix by inverse (matrix)
        setinverse <- function(inverse) {
                m <<- inverse
        }
        
        # Get the matrix information
        getinverse <- function() {
                m
        } 
        
        # Return the list consisting of 4 matrices
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Purpose:              To find Inverse Matrix matrix if it is required. 
#                       If its infomation is set before, return the cache information
# Function argument:    x is the matrix which inverse will be calculated if required.
# Return:               Inverse matrix of x
# Assumption:           Input matrix is invertable


cacheSolve <- function(x, ...) {
        
        # Get Inverse of 'x'
        m <- x$getinverse()
        
        # If Inverse was calculated before, m is with Inverse Matrix which will be returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If Inverse was NOT calculated before, calculate Inverse Matrix which will be returned
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        
        return (m)
}
