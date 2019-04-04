## Caching the Inverse of a Matrix
## ---------------------------------------------------------------------
## There are many ways of finding the inverse of a matrix yet
## it is usually a costly computation. In addition, caching the
## inverse of a matrix may be beneficial than computing it repeatedly.
## The pair of functions below are used to create a special object that 
## essentially stores a matrix and caches its inverse.
##----------------------------------------------------------------------

##----------------------------------------------------------------------
## The first function ("makeCacheMatrix") creates a special "matrix"
## object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(a) {
    x <<- a
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##----------------------------------------------------------------------


##----------------------------------------------------------------------
## The second function ("cacheSolve") computes the inverse of the 
## returned special "matrix" by the first function. This function has
## the constraint that if the inverse has already been calculated (and 
## the matrix has not changed), then it should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

##----------------------------------------------------------------------
