## calculate the inverse of a matrix. The inverse of matrix will be cached, for a faster result without calculation next time.

## makeCacheMatrix set, get the matrix, and then set the get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the Matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the Matrix
  get <- function() x
  # set the inverse of Matrix
  setInverse <- function(solve) m <<- solve
  # get the inverse of Matrix
  getInverse <- function() m
  # print
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculate the inverse of matrix defined by makeCacheMatrix.First check whether it has been calculated and stored.

cacheSolve <- function(x, ...) {
  # first check to see whether the inverse matrix has been calculated and cached, with getInverse
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if not cached, next the inverse of matrix will be calculated with solve function
  # and store the result with setInverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

