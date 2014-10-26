## There are two functions in this script file. makeCacheMatrix contains
## the getter/setter methods for a matrix and its inverse. cacheSolve 
## function on the other hand computes the inverse of the matrix and 
## caches it using the makeCacheMatrix function. 

## This function has the getter/setter for matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of a matrix. caches it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix

}
