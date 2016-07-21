## This script contains two functions as required by the assignment.
## The first function handles all communication with the cache and is used to set the matrix,
## while the second function computes the inverse.

## This function constructs a list containing functions to 
##   - retrieve a matrix from cache,
##   - write the matrix provided by the user to cache,
##   - retrieve the cached intverted matrix
##   - write the inverted matrix to cache.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function calculates the inverted matrix or retrieves it,
## if it exists, using the first function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## This is a minimal working example:
## 1. Set matrix (upper diagonal square matrices are always invertible)
# mat <- makeCacheMatrix(matrix(data=c(1,1,0,1), ncol=2))
## 2. Invert matrix
# cacheSolve(mat)
## 3. Test result
# cacheSolve(mat)%*%mat$get()