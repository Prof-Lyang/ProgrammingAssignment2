## Coursera, R Programming, April 2014
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The assignment is to write a pair of functions that cache 
## the inverse of a matrix.   

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse.
  #
  # Args:
  #   x: The matrix which inverse to be calculated
  # 
  # Returns:
  #   The object that can cache the inverse of the matrix.  

  inverse.x <- NULL  # hold the inverse of a matrix
  set <- function(y) {
    # set the value of the matrix and clear the value of inverse matrix
    # The "<<-" operator used to set variable that already exists 
    # in the parent environment.
    x <<- y
    inverse.x <<- NULL
  }
  get <- function() x  # Get the value of the matrix.
  setinverse <- function(inv) {
    # Set the value of the inverse matrix.
    inverse.x <<- inv   
  }
  getinverse <- function() inverse.x  # Get the value of the inverse matrix.
  # create the list to access properties with $ sign. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

cacheSolve <- function(x, ...) {
  # Calculates matrix inverse for makeCacheMatrix object.
  #
  # Args:
  #   x: The makeCacheMatrix object.
  # 
  # Returns:
  #   The inverse matrix.
  #
  inverse.x <- x$getinverse()  # Get the cached value of inverse matrix.
  if(!is.null(inverse.x)) {  # if cached value exists then return it.
    message("getting cached data")  # notify user 
    return(inverse.x)  # return inverse matrix
  }
  # Cached value of inverse matrix is not exists. We will caclulate it
  data <- x$get() # Get the matrix to inverse 
  inverse.x <- solve(data, ...)  # Inverse matrix
  x$setinverse(inverse.x)  # Store the result in the makeCacheMatrix object
  inverse.x  # return inverse matrix
  
}

# test
# m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# cacheSolve(m)
# cacheSolve(m)
