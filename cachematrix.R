## These two functions wiil compute the inverse of "matrix" . 
## Only invertible matrix is in scope for this exercise.
## Generally computing inverse of a matrix is a complicated & time cosuming calculations
## To reduce the running time, we have used cache data, 
## which should be an optimum solution for this problem.

## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- inv
  getinv <- function() i
  list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## The function 'cacheSolve' computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- i$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
