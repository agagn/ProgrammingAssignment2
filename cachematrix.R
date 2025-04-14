## Programming Assignment 2: Lexical Scoping
## a pair of functions that cache the inverse of a matrix

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function()
    x
  
  setsolve <- function(solve)
    s <<- solve
  
  getsolve <- function()
    s
  
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## Try to get the result from the cache.
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## Reverse the matrix and set the cache
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  ## Return the inverse matrix  
  s
}
