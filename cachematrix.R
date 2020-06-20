## Pair of functions that compute and cache the inverse of a matrix.

## makeCacheMatrix:  creates a special "matrix", which has a list with a function to
#  - set the value of the matrix
#  - get the value of the matrix
#  - set the value of the inverse of the matrix
#  - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cachesolve: computes the mean of the special "matrix" created with makeCacheMatrix. 
# - first tries it can get the inverse from the cache and skip the computation. 
# - otherwise, it calculates the inverse of the data and set the inverse in the cache.

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

