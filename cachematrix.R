## The following two functions compute the inverse of a numeric matrix.
## The inverse is cached on first computation, so that it can be looked up 
## rather than recomputed later.

## makeCacheMatrix description:
## Input: numeric matrix
## Output: list of 4 functions to:
##  set value of matrix
##  get value of matrix
##  set value of matrix inverse
##  get value of matrix inverse
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


## cacheSolve Description:
## Input: object created by makeCacheMatrix()
## Output: Inverse of the matrix input to makeCacheMatrix()
##
## Inverse is computed if it is not already cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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


