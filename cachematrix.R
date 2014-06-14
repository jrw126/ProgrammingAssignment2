## This pair of functions can be used to do the following
## 1. Create a special "matrix" object that can cache the inverse of the matrix.
## 2. Compute the inverse of the special "matrix" object.
## If the inverse has already been calculated (and hasn't changed),
## then the inverse will be retrieved from the cache.
## These functions are helpful if you want to cache potentially
## time-consuming computations.

## makeCacheMatrix creates the matrix object and caches the inverse,
## as described above.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## cacheSolve computes the inverse of vthe matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated,
## (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
