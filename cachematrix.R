## Functions in this file control the caching of the inverse of a matrix.
## The inverse of a matrix is only computed when requested.

## This function makes an list of functions that carry out operations on the original matrix.
## Functions in the list wrap around x (the original matrix) and inverse (the inverse of the original matrix).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculated the inverse of our special matrix.
## If the inverse of our matrix has not been calculated yet, it is calculated.
## Then the inverse is set on our special matrix, finally the inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
