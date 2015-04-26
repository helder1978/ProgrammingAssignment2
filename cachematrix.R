## This functions belong to the Programming assignment 2, 
## from the course R Programming of coursera
##
## The functions calculate the inverse of a Matrix (which can be a costly operation), 
## cache it and load it if requested later
##

## The makeCacheMatrix function creates a special "matrix" object
## and defines the object functions to aid the cache/load functionality
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The function cacheSolve calculates and caches the inverse of the "matrix" object
## if it wasn't yet calculated or loads it from cache otherwise.
cacheSolve <- function(x, ...) {
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
      message("getting cached data")
      # loads it from cache 
      return(m)
    }
    data <- x$get()
    # calculates and caches the inverse of the "matrix" object
    m <- solve(data, ...)
    x$setInverseMatrix(m)

    ## Return a matrix that is the inverse of 'x'
    m  
}
