## Put comments here that give an overall description of what your
## functions do

# Since matrix inversion is costly computation, the following functions 
# cache the inverse of matrix, to avoid re-calculation if already available

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse
# using lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  set <- function(y) {
    x <<- y                 #assign to object in parent env
    mat <<- NULL            #clear the object in parent env
  }
  
  get <- function() x
  setSolve <- function(solve) mat <<- solve #assign value from parent env
  getSolve <- function() mat
  
  # create the list of named fuctions to be accessed later
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x$getSolve()
  
  # Check if result is already in cache, return the same if present
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  # Get the x and store its inverse in mat object
  dat <- x$get()
  mat <- solve(dat, ...)
  x$setSolve(mat)
  
  ## Return a matrix that is the inverse of 'x'
  mat
}
