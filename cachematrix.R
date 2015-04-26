## Coursera's R Programming Course - Programming Assignment 2
## Functions:
##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   cacheSolve: This function computes the inverse of the special "matrix" returned by
##   makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
##   changed), then the cachesolve should retrieve the inverse from the cache.


## This function initializes internal variable 'matrixCache' with the value of 'matrixCache' argument
## and defines set/get methods for setting/getting cache of matrix and of inverse matrix values

makeCacheMatrix <- function(matrixCache = matrix()) {
  
  # Inicialization
  inverseMatrix <- NULL
  matrixCache <- matrixCache
  
  # Matrix set/get
  set <- function(m){
    matrixCache <<- m
    inverseMatrix <<- NULL
  }
  get <- function() matrixCache
  
  # Inverse matrix set/get
  setInverse <- function(m) inverseMatrix <<- m
  getInverse <- function() inverseMatrix
  
  # List of 'Methods'
  list(get=get, set=set, setInverse=setInverse, getInverse=getInverse)
}


## This function computes inverse matrix for the special "matrix" object returned by the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  
  # Gets cache inverse matrix
  m <- x$getInverse()
  
  # Checks if it's already calculated.
  if (!is.null(m)) {
    
    #If it's calculated, don't need to calculate again, so returns that value
    print("Getting cached inverse matrix")
    return(m)
    
  }
  
  # If it's not calculated, it gets the matrix cache value
  data<-x$get()
  # Calculate inverse matrix
  m<-solve(data,...)
  # Cache inverse matrix
  x$setInverse(m)
  # Return inverse matrix value
  m
  
}