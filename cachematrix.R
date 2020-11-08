## calculate the inverse of a matrix. The inverse of matrix will be cached, for a faster result without calculation next time.

## makeCacheMatrix set, get the matrix, and then set the get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculate the inverse of matrix defined by makeCacheMatrix. First check whether it has been calculated and stored.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

