## Put comments here that give an overall description of what your
## functions do
# These two functions take a matrix that can cache it's inverse and computes the inverse, if it has
# not already been done.

## Write a short comment describing this function
# makeCacheMatrix creates a matrix object which is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

## Write a short comment describing this function
# This function returns the inverse of a matrix. First, it checks if the value has already 
# calculated, in which case, it returns the cachematrix. And if not, it creates the cachematrix

cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getCache()
  if (!is.null(cacheMatrix)) {
    message("getting cacheMatrix")
    return(cacheMatrix)
  }
  else {
    dataMatrix <- x$getMatrix()
    cacheMatrix <- solve(dataMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}