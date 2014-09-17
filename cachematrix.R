## Pair of functions that cache the inverse of a matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initializes the inverse variable
  inv <- NULL

  ## Function to set the matrix
  set <- function(m) {
    x   <<- m
    inv <<- NULL
  }

  ## Function to get the matrix
  get <- function() {
    x
  }

  ## Function to set the inverse of the matrix
  setInverse <- function(i) {
    inv <<- i
  }

  ## Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }

  ## Returns a list of the functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Returns a matrix that is the inverse of 'x'

  ## Retrieves the inverse of the matrix from the cache
  i <- x$getInverse()

  ## If the invere has already been calculated, we return the value
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }

  ## If it hsn't been calculated, gets the matrix from the 'x' object
  m <- x$get()

  ## Calculates the inverse using matrix multiplication
  i <- solve(m, ...)

  ## Sets the inverse in the object
  x$setInverse(i)

  ## Returns the inverse
  i

}
