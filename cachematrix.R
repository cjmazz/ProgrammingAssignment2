## This source file defines two functions that enable the inverse of a matrix to be obtained and 
## locally cached for efficiency.  The functions are described in more detail below

## This function 'makeCacheMatrix' creates a special kind of "matrix" that allows the the inverse of a specified matrix (x)
## to be obtained and cached locally.  This function assumes the specified matrix x is invertible - if it is not, an error is thrown.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##  setMatrix is sed to set a new matrix as the original matrix to be inverted.
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##  getMatrix just returns the matrix passed in to the function or set via setMatrix
  getMatrix <- function() x
  ##  setInverse sets the inverse matrix to 'm'
  setInverse <- function(invm) m <<- invm
  ##  getInverse gets the inverse matrix 'm'.  This may be null meaning the inverse matrix of 'x' has not been calculated.
  getInverse <- function() m
  ##  This function makeCacheMatrix returns a list of the four functions specified above.  This is referred to as a 'special' matrix.
  list(set = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function 'cacheSolve' creates the inverse of the matrix established in 'makeCache Matrix' if it does not already
## exist or pulls up the cached value if it has already been calculated.
## The x argument is a 'special' matrix defined by calling makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ##   Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ##   Check to see if m is null, if it is not, it means this has been calculated already, so return the previously calculated value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##  Otherwise get the inverse of the matrix and set it via the setInverse function.
  mtx <- x$getMatrix()
  m <- solve(mtx)
  x$setInverse(m)
  ## return the inverse matrix
  m
}

