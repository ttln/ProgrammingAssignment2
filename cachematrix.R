# Programming assignment.
# This pair of functions caches the inverse of a square matrix.
# 'makeCacheMatrix' must be called first to make a cacheable matrix.
# Then 'cacheSolve' must be called with the cacheable matrix.
# Every subsequent call to 'cacheSolve' for the same cacheable matrix
# will not compute the inverse again but use the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Makes a cacheable matrix for 'cacheSolve'.
  #
  # Args:
  #   x: A square matrix.
  #
  # Returns:
  #   A special square matrix (a list of functions) usable with 'cacheSolve'.
  
  inverse <- NULL
        
  set <- function(y) {
    # Returns an assignment of 'NULL' for 'inverse' to one of
    # the caller's parent environments.
    x <<- y
    return (inverse <<- NULL)
  }
  
  get <- function() {
    # Returns this function's parent variable 'x',
    # the matrix used to build the cacheable matrix.
    return (x)
  }
  
  SetInverse <- function(solve) {
    # Returns an assignment of the inverse of the matrix to one of
    # the caller's parent environments.
    return (inverse <<- solve)
  }
  
  GetInverse <- function() {
    # Returns this function's parent variable 'inverse'.
    return (inverse)
  }
  
  function.list <- list(set=set,
                        get=get,
                        SetInverse=SetInverse,
                        GetInverse=GetInverse)
  
  return (function.list)
}

cacheSolve <- function(x, ...) {
  # Returns the inverse of a square matrix.
  #
  # Args:
  #   x: A special square matrix. It MUST have been made by 'makeCacheableMatrix'
  #
  # Returns:
  #   The inverse of the square matrix (if possible, from cached data).
  
  inverse <- x$GetInverse()
  
  # Uses the cached inverse of the matrix if available.
  if(!is.null(inverse)) {
    
    message("Getting cached matrix.")
    return(inverse)
    
  } else {
  
    # Get the matrix, compute and cache its inverse.
    
    matrix.data <- x$get()
    inverse <- solve(matrix.data, ...)
    x$SetInverse(inverse)
    
    return (inverse)
  }
}
