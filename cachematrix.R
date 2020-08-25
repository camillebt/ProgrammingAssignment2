## Since calculating for the matrix inverse is costly, caching it will make it
## more efficient by returning values for an already calculated inverse

## This function creates a matrix object and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function will check if the inverse of the given matrix is already 
## calculated and return it if it is, calculate it if isn't
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setInverse(inv)
  inv
}

