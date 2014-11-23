## This function creates a special "matrix" object that can cache its inverse
## It is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) m <<- solution
  getsolution <- function() m
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data 
## and sets the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getsolution()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolution(m)
  m
}
