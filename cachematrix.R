# makeCacheMatrix: return a list of functions to:

# 1. set : Set the value of the matrix
# 2. get: Get the value of the matrix
# 3. setInverse: Set the value of the inverse
# 4. getInverse: Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # m will store the cached inverse matrix 
  
  m <- NULL
  
  
  #Setter for the matrix 
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Getter for the Matrix
  
  get <- function() x
  
  #setter for the inverse
  setInverse <- function(inverse) m <<- inverse
  
  #Getter for the inverse 
  getInverse <- function() m
  
  
  
  #return the matrix with 4 functions as a list
  list(set = set, get = get,       setInverse = setInverse,       getInverse = getInverse)
  
}


# cacheSolve: Compute the inverse of the matrix. 
# If the inverse is already calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  
  # If the inverse is already
  # calculated before, it returns the cached inverse.  
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  
  # The inverse is not yet calculated, so we calculate it
  
  data <- x$get()
  m <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(m)
  
  # Return it
  m
}

