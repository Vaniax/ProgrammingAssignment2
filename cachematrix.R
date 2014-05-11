## The followning functions "makeCacheMatrix" and "cacheSolve" can be used
## to calculate the inverse of a matrix. The result is stored in a cache to 
## avoid unnecessary recalculation of the inverse when the matrix does not change.

## The following function creates a list that contains different
## functions:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the matrix inverse
## 4) Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse matrix created
## by "makeCacheMatrix". If the inverse was calulated before, it 
## skips the calculation and returns the cached result.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
