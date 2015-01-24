# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# This script contains two functions are used to cache the inverse of a matrix.


#The first function, makeCacheMatrux creates a special 
#"matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#setsolve the value of the solve
#getsolve the value of the solve

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
       setsolve = setsolve, getsolve = getsolve)
}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data.")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
