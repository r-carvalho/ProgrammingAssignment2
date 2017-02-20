##  R programming Course - Programming Assignment 2: Lexical Scoping
##  By Rafael V. Carvalho
##  The functions implemented here are used to compute and cache de inverse of a matrix
##  We assume that the matrix supplied is always invertible.

##  This function create a special matrix containing specific functions to
##  Set and Get the values of the matrix;
##  Set and Get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    imatrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(invmatrix) imatrix <<- invmatrix
  getinverse <- function() imatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##  This function returns the inverse of the matrix supplied
##  It verify if the inverse of the matrix was already computed and stored in cache 
##  If so, it gets the result and skip computation. If not, it computes the inverse
##  and set the value in the cache

cacheSolve <- function(x, ...) {
  imatrix <- x$getinverse()
  
  if (!is.null(imatrix)) {
    message("Getting cached data")
    return (imatrix)
  }
  
  data <- x$get()
  imatrix <- solve(data, ...)
  x$setinverse(imatrix)
  imatrix
  
}
