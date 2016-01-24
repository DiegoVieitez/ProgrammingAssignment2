## These functions allow to store in cache the inverse of a matrix.
## This avoids needless repetions of this operation, therefore saving time.

## The first function, makeCacheMatrix, creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the result of the function above and, if the inverse of the
## matrix has not been calculated, it does so and caches it. Otherwise, it 
## retrieves the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
