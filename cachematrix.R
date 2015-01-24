## These functions are used to create special object that stores a matrix and caches its inverse

## Creates special "matrix", with really is a list containing a function to
## - set/get the value of matrix
## - set/get the value of inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates inverted matrix of the special "matrix" created with the makeCacheMatrix function.
## First checks to see it the inverted matrix has already been calculated and returns it if so.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached result")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
