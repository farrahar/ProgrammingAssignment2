## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mat <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mat);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## This function computes the inverse of the special "matrix" returned 
## by 'makeCacheMatrix' above. If the inverse has already been
## calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinverse(inverse)
  return(inverse)
}

########## Sample run ##########

# > m <- matrix(rnorm(25), nrow = 5)          // Create a matrix x
# > cm <- makeCacheMatrix(m)                  // Create our special matrix
# > cm$get()                                  // Return the matrix
# > cacheSolve(cm)                            // Return the inverse
# > cacheSolve(cm)                            // When we call the 2nd time,
#                                             // it returns the cached inverse