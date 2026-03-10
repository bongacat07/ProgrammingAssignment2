
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x   <<- y
    inv <<- NULL        # reset cache when matrix changes
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: returns the inverse of the matrix, using cache if available
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
