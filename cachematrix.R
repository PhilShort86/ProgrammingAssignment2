## R Programming Assignment 2: Lexical Scoping
## The following two functions, makeCacheMatrix and cacheSolve, creates a spercial matrix and solves for the inverse 
## of a matrix if there is no previous solution that has been cached.
## Example output: 
##>Test_Matrix <-makeCacheMatrix(matrix(c(2,0,0,1), c(2,2)))
##> cacheSolve(Test_Matrix)
##  [,1] [,2]
##  [1,]  0.5    0
##  [2,]  0.0    1
##> cacheSolve(Test_Matrix)
##  getting cached data
##  [,1] [,2]
##  [1,]  0.5    0
##  [2,]  0.0    1


## Create a special matrix, which is a list containing function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
       set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve solves for the inverse of the matrix created in makeCacheMatrix after to checks to see if the inverse of the matrix 
## has already been determined.  If the matrix has already been determiend, it returns the cached data for the matrix and skips
## reperforming the inverse on the matrix again.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}