## using <<- operator data is being stored in alternative environment
## data is cached

makeCacheMatrix <- function(x = matrix()) {
  
  m <- matrix() ## m - inverted matrix in current environment
  
  set  <- function(y) {   ##cache writer
    x <<- y               ##put this matrix in alternative environment
    m <<- matrix()        ##clear inverted matrix values in alternative environment
  }
  
  # get the matrix origin from another space
  get <- function() x
  # set matrix inversion values in another space
  setminv <- function(minv) m <<- minv
  # get matrix inverstion values from another space
  getminv <- function() m
  # printout
  list(set = set, get = get, setminv = setminv, getminv = getminv)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getminv()
  if(!all(is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminv(m)
  m
  }