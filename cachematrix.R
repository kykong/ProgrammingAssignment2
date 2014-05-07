## The following 2 functions operate on a matrix, to store and return the matrix and
## its inverse (if any), and to calculate that inverse matrix.

## The makeCacheMatrix function takes in a matrix as
## an argument, and returns a list of functions.  The functions are:
## get():  return the matrix
## set(y):  set a new value for the matrix
## getInverse():  return the cached inverse of the matrix if available, or NULL otherwise
## setInverse(y): sets the value of the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(y) inv <<- y
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function takes in the list returned by the makeCacheMatrix
## function as an argument, and checks whether there is cached (i.e. non-null)
## inverse of the matrix.  If the cached value exists, then it just returns the 
## cached value.
## Otherwise, it calculates the inverse of the matrix, and stores it within
## the original list structure (which was provided as the function argument).
## KEY ASSUMPTION:  the matrix is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
