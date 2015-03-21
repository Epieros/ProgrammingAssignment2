## These functions are meant to set and retrieve a matrix and
## its inversion in a locally cached variable

## This function is meant to do the following:
## 1) set: accepts a matrix as input and stores it in cache
## 2) get: retrieves the original matrix from cache
## 3) setinverse: accepts the inversion of the original matrix 
##                as input and stores it in cache
## 4) getinverse: retrieves the inversion matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    ## Clear the inversion matrix cache
    inv <- NULL
    
    ## Set the original matrix to cache and ensure the inversion is clear
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Retrieve the original matrix from cache
    get <- function() x
    
    ## Create the inversion matrix in cache
    setinverse <- function(solve) inv <<- solve
    
    ## Retrieve the inversion matrix from cache
    getinverse <- function() inv
    
    ## Show functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function accepts another function representing makeCacheMatrix
## and will return an inversion matrix from cache if one exists
## or it will create the inversion matrix, setting the result to cache
## and returning the result to the console

cacheSolve <- function(x, ...) {
    ## Add current value of inversion matrix to local variable
    inv <- x$getinverse()
    
    ## Check to see if the inversion matrix has been created,
    ## if so, return the existing inversion matrix to console
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the current inversion matrix value is empty 
    ## create the inversion matrix, add the result to cache
    ## and return result to console
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

