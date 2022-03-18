## Put comments here that give an overall description of what your
## functions do
# Caching inverse of a matrix instead of calculating it repeatedly to save the time and effort.

## Write a short comment describing this function
#"makeCacheMatrix" is creating a matrix to set and get matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invers <<- inverse
  getinverse <- function() invers
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# "cacheSolve" function is used to calculate the inverse of matrix and sets inverse matrix in cache. If the inverse of matrix is already present it fetches the inverse from cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  matrixdata <- x$get()
  invers <- solve(matrixdata, ...)
  x$setinverse(invers)
  invers
}
