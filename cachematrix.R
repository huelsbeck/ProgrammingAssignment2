## These functions create and manipulate a special matrix object, for which the
## inverse is stored ("cached") once computed, saving unnecessary recompuation.

## 'makeCacheMatrix' takes what is presumed to be a square, invetable matrix
## and returns a list, the first element of which 'm' is the matrix itself.
## The remaining elements of the list are functions for manipulating the 
## inverse of the matrix, which are used by the companion function 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  id <- NULL
  m <- x
  
  geti <- function() i
  seti <- function(inv) i <<- inv
  getid <- function() id
  setid <- function(dd) id <<- dd
  
  return(list(m = m, geti = geti, seti = seti, getid = getid, setid = setid))
}


## 'cacheSolve' is the companion function to 'makeCacheMatrix'. It accepts the
## list output from 'makeCacheMatrix' and returns the inverse, presuming that
## the inverse is defined. If 'cacheSolve' is called more than once for a given
## matrix, the stored inverse will be returned provided that the elements of 
## the matrix have not been altered since the previous call to 'cacheSolve'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i) && identical(x$m,x$getid())) {
    message("returning cached inverse")
    return(i)
  }
  x$setid(x$m)
  i <- x$seti(solve(x$m))
  return(i)
}
