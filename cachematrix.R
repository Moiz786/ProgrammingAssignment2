## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a matrix object that can cache its inverse
##TEST COMMANDS:
##matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##matrix$get() 
##matrix$set(matrix(2:5, 2, 2))
##matrix$get()
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of matrix returned by makeCacheMatrix()
##TEST COMMANDS:
##matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##matrix$get() 
##cacheSolve(matrix)
##cacheSolve(matrix)
##matrix$getInverse()
##matrix$set(matrix(2:5, 2, 2))
##matrix$get()
##cacheSolve(matrix)
##matrix$getInverse()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
  
}
