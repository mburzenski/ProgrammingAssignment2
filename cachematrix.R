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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("get cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
