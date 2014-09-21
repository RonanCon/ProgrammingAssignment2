## Creates a matrix object that can calculate its own inverse (a)
## and caches that value which can be recalled until the input 
## matrix is changed

## This function creates the object which holds the input matrix
## and defines the variables and functions required to cache the 
##inverse of the input matrix. If the input matrix changes this
##function resets the cached value

makeCacheMatrix <- function(x = matrix()) { 
            a <- NULL ## sets the inverse value to NULL
            set <- function(y) { ## substitutes input matrix y for the original x
                    x <<- y 
                    a <<- NULL ## resets the inverse to NULL if input matrix changes
            }
            get <- function() x ## retrieves the input matrix
            setinverse <- function(solve) a <<- solve ##writes the inverse to the cache
            getinverse <- function() a ## retrieves the inverse from the cache
            list(set = set, get = get, ##creates list holding the functions
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function calls the value of the inverse from the cache. 
##If its a non-NULL value it prints it and if its NULL it calculates 
##the inverse of the input value

cacheSolve <- function(x, ...) {
        a <- x$getinverse() ## checks the cache for the inverse
        if(!is.null(a)) { ## prints the value of inverse in cache if its not NULL
           message("getting cached data")
           return(a)
            }
            data <- x$get() ## creates a variable data which contains the input matirx
            a <- solve(data, ...) ##calcaulates the inverse
            x$setinverse(a) ## writes to the cache
            a     ##prints the inverse
}
