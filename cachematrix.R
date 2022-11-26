# The inverse of a matrix can be cached rather than being computed repeatedly
# because matrix inversion is typically an expensive process. 
# The inverse of a matrix is cached using the next two functions written below.

## makeCacheMatrix creates a list containing a function to
## set and get the value of the matrix
## set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




# The inverse of the matrix is returned by the next function. 
# First, it determines whether the inverse has previously been calculated. 
# If so, it obtains the outcome and forgoes the calculation.
# If not, it calculates the inverse and uses the set inverse function to set the value in the cache.

# This function assumes that the matrix is always invertible.


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
