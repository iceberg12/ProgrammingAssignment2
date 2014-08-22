## Cache the inverse of a Matrix

## Create a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)  
}

## Retrieve the inverse if it is cached.
## Otherwise, compute the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
