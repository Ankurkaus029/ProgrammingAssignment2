## Put comments here that give an overall description of what your
## functions do

## The first function, makeVector creates a special "vector", which is really a list containing a function to set the value of the Matrix get the value of the Matrix 
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinv <- function(solve) k <<- solve
  getinv <- function() k
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
k <- x$getinv()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data)
  x$setinv(k)
  k
}
