## Create a special matrix and compute the inverse of Matrix. If the inverse is already computed, fetch the cached result.

## Function to create special Matrix, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL               # Initialize Inverted Matrix to NULL
  set <- function(y){             
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(y) invMatrix <<- y
  getInvMatrix <- function() invMatrix
  
  ## Return list of functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Computes Inverse of ther special matrix. If already calculated earlier it fetches the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getInvMatrix()
  if (!is.null(y)) {
    message("Getting cached data")
    return(y)
  }
  
  data <- x$get()
  invertedMatrix <- solve(data, ...)
  x$setInvMatrix(invertedMatrix)
  invertedMatrix
}