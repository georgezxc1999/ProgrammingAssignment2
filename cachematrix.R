## The makeCacheMatrix function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        ## set the value of the matrix
        x <<- y  
        i <<- NULL
    }
    get <- function() x    ## get the value of the matrix
    setinverse <- function(inverse) i <<- inverse     ## set the value of the inverse
    getinverse <- function() i    ## get the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix function 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {   ## check if inverse has already been calculated
        message("getting cached data")
        return(i)   ## retrieve the inverse from the cache
    }
    data <- x$get()  ## get the value of matrix
    i <- solve(data, ...)  ## calculate the inverse
    x$setinverse(i)  ## store inverse in cahce
    i
}
