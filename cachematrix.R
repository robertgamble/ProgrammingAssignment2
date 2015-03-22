## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## These are a pair of functions that cache the inverse of a matrix.

## Assignment part 1 - makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(cachedMatrix = matrix()) {
  # This makes x equal to empty matrix
  I <- NULL
  # This sets inverse to null
  
  set <- function(y){
    x <<- y
    # assigns the argument to x
    I <<- NULL
    # inverse is set to null
  }
  get <- function() x
  # returns matrix value
  
  setInverse <- function(solve) I <<- solve
  # overrides previous valueand assigns inverse
  
  getInverse <- function() I
  # returns inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # returns function list
  
}

## Assignment part 2 - cachesolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(matrixCache = makeCacheMatrix, ...) {
  # check if cache has existing inverse value
  matrixInverse <- matrixCache$getInverse()
  
  cacheSolve <- function(x, ...) {
    I <- x$getInverse()
    # gives the most recent value for the inverse
    
    if(!is.null(I)){
      message("gettingcached data")
      return(I)
      # cache returns value if not null         
    }
    # If null, retrieve matrix x and calculate inverse
    message("newly calculating data")
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    
    # This sets inverse to calculated value   
    I #returnsinverse value
}