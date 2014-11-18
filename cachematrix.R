## Creates a special matrix that can cache the inverse of the matrix.
## function will set the matrix, get the matrix, return the matrix,
## set the inverse of the matrix, get the matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
   n <- NULL
   set <- function(matrix) {
      x <<- matrix
      n <<- NULL
   }
   get <- function() {
      x
   }
   setInverse <- function(inverse) {
      n <<- inverse
   }
   getInverse <- function() {
      n
   }
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Calculates the inverse of the special matrix of function makeCacheMatrix.
## If already calculated then cacheSolve will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
   n <- x$getInverse()
   if(!is.null(n)) {
      message("getting cached data")
      return(n)
   }
   data <- x$get()
   n <- solve(data, ...)
   x$setInverse(n)
   n
}
