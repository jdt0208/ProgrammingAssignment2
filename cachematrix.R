## Together makeCacheMatrix and cacheSolve provide the user
## with a set of functions to create, modivy and retrieve a matrix
## and its inverse.  Once the inverse of the matrix is calculated,
## it is cached in memory for future retrieval without the need
## for recalculation.

## makeCacheMatrix returns a set of functions which operate on a 
## matrix object to set its values, return its values, set its 
## inverse (meant to be called from cacheSolve below) and return
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  setmatrix<- function(y){
    x<<- y
    m<<- NULL
  }
  getmatrix<- function() x
  setinverse<- function(inv) m <<- inv
  getinverse<- function() m
  list(setmat = setmatrix, getmat = getmatrix, setinv = setinverse, getinv = getinverse)
}


## CacheSolve returns the inverse of the matrix associated with
## makeCacheMatrix. If the matrix is newly created, whether through
## a call to setmatrix or a newly executed call to makeCacheMatrix,
## then the inverse is calculated.  If the inverse has already been
## previously, then the inverse is returned from memory.

cacheSolve <- function(x, ...) {    
  m<- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<- x$getmat()
  m<- solve(matrix, ...)
  x$setinv(m)
  m
}
