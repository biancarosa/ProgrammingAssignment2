## Creates a matrix and caches its inverse in the first time that calculates it

## For a matrix x, we will setup a few methods to make its inverse cacheable
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL;
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL;
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getinverse <- function() {
    cachedInverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse only if it has not been cached
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
