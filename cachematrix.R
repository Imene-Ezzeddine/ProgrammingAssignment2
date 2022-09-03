##This function gives the inverse of a special matrix
## and  cash it rather than compute it repeatedly.


## This function creates a special "matrix" 
##object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse 
##of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}
