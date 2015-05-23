
# Comments Begin
# Following the "makeVector" example we create "makeCacheMatrix"
# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# Comments End

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverseInput) inverse <<- inverseInput
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Comments Begin
# Following "cachemean" example we create "cacheSolve"
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache
# Comments End

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data.")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  inverse
}
