## Defines CacheMatrix object that caches its inverse and 
## cacheSolve function that returns the inverse of a CacheMatrix.

## makeCacheMatrix: defines a matrix with default value NULL and the option to
## cache the inverse, with setters and getters for both.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Attempts to retrieve the cached inverse of CacheMatrix m.  If that
## fails, calculates the inverse and stores it in m.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
