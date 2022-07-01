## Programming Assignment 2: Lexical Scoping
## Creates a special "matrix" object that stores its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## Creates a list of functions that allows getting and setting the matrix,
  ## As well as getting and setting the inverse of the matrix.
  ##
  ## Parameters:
  ##   x (matrix) - The value of the matrix
  ##
  ## Returns: A list of functions representing the cache matrix
  
  inv <- NULL
  
  # Set the value of the matrix to a matrix y and delete the current cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() {
    x
  }
  
  # Set the inverse of the matrix to a matrix inv
  set_inv <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the inverse of the matrix
  get_inv <- function() {
    inv
  }
  
  # Return the cache matrix
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


cacheSolve <- function(x, ...) {
  ## Calculate and return the inverse of the cache matrix.
  ##
  ## Parameters:
  ##   x (list) - The cache matrix object
  ##
  ## Return: The matrix inverse
  
  # Get the current value of the matrix
  inverse <- x$get_inv()
  
  # If the inverse is not yet calculated, calculate it.
  if (is.null(inverse)) {
    inverse <- solve(x$get(), ...)
    x$set_inv(inverse)
  }
  
  # Return the matrix inverse
  inverse
}
