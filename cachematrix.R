## This document has the functions to create a cache for matrices which
## are re-run frequently. Create a cache with makeCacheMatrix and make use
## of it with the cacheSolve function for getting inverse matrix.

## This function gives access to the Matrix cache for faster computation

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        # Create setter functionality
        set <- function(y) {
                x <<- y
                inv <<- NULL # make sure we return NULL if a new matrix is assigned
        }
        
        # Getter functionality for variable x in this environment
        get <- function() x
        
        # Setter and getter for the solve function
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        # return the 'object' with set and get commands
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}


## This function computes the inverse of a Matrix and uses a cache for 
## faster computation when solving multiple times

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # if a cache exists for this matrix retrieve it
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # this line returns the value and ends the function
        }
        
        ## else no cache exists - calc the inv value and cache it
        # get the base matrix
        data <- x$get()
        # inverse the matrix
        inv <- solve(data, ...)
        # store it in cache
        x$setinv(inv)
        inv
}
