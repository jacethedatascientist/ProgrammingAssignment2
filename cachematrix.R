## Caching the Inverse of a Matrix
## ---------------------------------------------------------------------
## The one below is one of the ways to acquire the inverse of a matrix
## Attached below are the functions to create an object that 
## creates the matrix and caches its inverse.
##----------------------------------------------------------------------

## The first function ("makeCacheMatrix") creates a special "matrix".
## Stores the matrix created and caches its inverse.

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

## The second function ("cacheSolve") computes the inverse of the 
## special "matrix" from the first function. But there is a condtion 
## that if the inverse has already been computed but the matrix has 
## not changed, then it should print the inverse from the cache

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
