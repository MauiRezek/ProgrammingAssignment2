rm(list = ls())

## 2x2 square matrix
mat <- matrix(c(1,2,3,4),nrow = 2,ncol = 2)

## x <- makeCacheMatrix(mat) will initialize the special matrix "mat" (defined above)
## x$get() returns whatever matrix was initialized and stored in x
## x$set(matrix) will load a new matrix "matrix" into variable x 
## x$getInverse() will return the inverse of the matrix stored in x. 
## NOTE: If inverse has not been calcuated yet x$getInverse() will return NULL
## x$setInverse(mat) will calculate the inverse of the matrix "mat" 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()){
    x <<- y
    m <<- NULL
  }
  # calculates the inverse of a matrix
  get <- function() x
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(x) will return the inverse of matrix stored in "x" where "x" is previousy defined.
## If x$setInverse(mat) was called earlier then cacheSolve(x) will return cached data.
## If x$setInverse(mat) was not called then cacheSolve(x) will calculate and return the inverse 
## of the matrix stored in x.

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
