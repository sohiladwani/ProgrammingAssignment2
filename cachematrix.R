## I created 2 functions, one for caching the matrix(first function) and another function to compute the inverse or find it in the cached data(second function)

## This function caches a matrix (x)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list (set = set, get = get,
        setmat = setmat,
        getmat = getmat)
}


##  This function computes the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if (!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmat(m)
  m
}