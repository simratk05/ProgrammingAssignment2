makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when the matrix is updated
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check for cached inverse
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # Compute inverse if not cached
  mat <- x$get()
  inv <- solve(mat, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}

