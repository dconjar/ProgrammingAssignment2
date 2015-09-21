## Save the values of a matrix "x" and its inverse to the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (val) {
    x <<- val
    i <<- NULL
  }
  get <- function () x
  setInverse <- function (inverse) i <<- inverse
  getInverse <- function () i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate the inverse of a matrix "x"

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
