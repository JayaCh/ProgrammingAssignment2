## makeCacheMatrix: Creates and returns the list of getMatrix, setMatrix, getInverse and setInverse functions.
## The superassignment operator is used in the setMatrix and setInverse methods to cache the
## values of input matrix and the inverse matrix.

## cacheSolve: Calculates the inverse matrix of the matrix, that is retrieved by makeCacheMatrix object.
## First checks if the inverse matrix is available and if so gets it from cache and returns it. 
## Otherwise calcluates the inverse matrix, cahces it and returns it. 

# makeCacheMatrix returns the list with the setMatrix, getMatrix, setInverse and getInverse functions.
makeCacheMatrix <- function(x = matrix()) {   # input x will be a matrix
  
  cachedInv <- NULL    
  
  # Cache the input matrix
  setMatrix <- function(y) {    
    x <<- y
    # Input matrix changed, hence the cachedInv needs to be reset to NULL
    cachedInv <<- NULL
  }
  
  # Retreive the input matrix
  getMatrix <- function() { x }    
  
  # Cache the inverse matrix
  setInverse <- function(inverse) { cachedInv <<- inverse } 
  
  # Retrieve the inverse matrix
  getInverse <- function() { cachedInv }
  
  # Return the list with the functions
  list( setMatrix = setMatrix,
        getMatrix = getMatrix,                
        setInverse= setInverse,  
        getInverse = getInverse)

}

# cacheSolve returns the inverse matrix of the matrix retrieved by makeCacheMatrix.
cacheSolve <- function(x, ...) {
      # input x will be makeCacheMatrix object
  
  # Retrieve the chached inverse matrix
  inverse <- x$getInverse()
  
  # If the cached inverse matrix is not NULL, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Retrieve the input matrix
  data <- x$getMatrix()
  
  # Calculate the inverse matrix
  inverse <- solve(data)
  
  # Cache the inverse matrix
  x$setInverse(inverse)
  
  # Return the calculated matrix
  inverse
}
