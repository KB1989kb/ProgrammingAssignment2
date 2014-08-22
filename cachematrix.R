## The functions create inverse matrices. Instead of recreating them in case of multiple execution of the commands
## (which sometimes takes a lot of time), results are cached and recalled if necessary.
## If there are no results yet, the second function creates and stores the result.

## This function is meant to produce a matrix for which cacheSolve() creates the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               # Inverse matrix is set to NULL.
        set <- function(y){     # Want another matrix?  
                x <<- y         # Matrix y is stored here.
                i <<- NULL      # Inverse is set to NULL.
        }
        get <- function() {x}   # Gets the original matrix.
        setinverse <- function(inverse) {i <<- inverse}   # If there is no inverse matrix yet, stores the new one here.
        getinverse <- function(){i}     # If there is an inverse matrix, gets it.
        list(set = set, get = get,      # Enables access to the elements of the function.
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks whether there is an inverse matrix for the input matrix yet 
## and if so, gets the cache with getinverse().
## If there is no inverse matrix yet, the inverse matrix is created and safed in setinverse().

cacheSolve <- function(x, ...) {
        i <- x$getinverse()     
        if(!is.null(i)){        # Checks whether an inverse matrix exists already.
                message("getting cached data")  # If this is the case, prints the cached inverse matrix.
                return(i)
        }
        data <- x$get()         # If there is no inverse matrix yet, the function gets the original matrix,
        i <- solve(data, ...)   # creates the inverse matrix
        x$setinverse(i)         # and stores it in the above function.
        i
}
