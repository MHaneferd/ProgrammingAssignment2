## The function cahches the inverse matrix internally.
## Based on the example for caching.
##
##
## It can be tested using the following commands in the console after you have sourced the file/script:
## You have to run the command cacheResolve twise to get the cached inversed matrix
##
## x <- matrix(c(13,21,11,42,51,63,74,85,9,8,7,6,5,4,3,2),4,4) # create a 4x4 matrix
## x # Look at the matrix
## mat <- makeCacheMatrix(x) # create the special matrix (list of functions)
## cacheSolve(mat) # Get the inversed matrix
## cacheSolve(mat) # Get the cached inverse matrix
##

## makeCacheMatrix creates a special matrix, which is a list containing functions to
## setting and getting the value of the matrix, and getting and setting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheResolve return the inverse matrix of x. 
## The function checks if the inverse is already cached, 
## if so, it will return the cached inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # Check if the inverse value is already cached
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    # calculate the inverse:
    m <- solve(data, ...)
    
    # Cache the inverse matrix
    x$setinverse(m)
    
    # Output the inverse matrix
    m
}
