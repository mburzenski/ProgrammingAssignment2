## Essentially these functions allowus to cache the inverse of a matrix and later recall the inverse 
## without performing calculations on each iteration. 

## This function creates a matrix from a given set of values. The inverse of the matrix is can be 
## cached in the global directory

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The code below checks to see if the inverse of the matrix has already been found, and if so it returns that value from the
##cache. If the inverse has not been found, the solve command is used to calculate and return it.

cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
