## Write a short comment describing this function
        #The makeCacheMatrix function below, creates a matrix that can cache
        #its inverse. This serves to reduce some of the cost of computation for
        #matrix inversion, which is usually highly costly.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function
        #This function is very similar to the example given in the assignment
        #instructions. It will compute the inverse of the matrix created using
        #the above makeCacheMatrix function OR retrieve an inverse from the
        #cache if it has already been computed.

cacheSolve <- function(x, ...) {
       m <- x$getInverse()
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setInverse(m)
       m
}
