## Put comments here that give an overall description of what your
## functions do

## This script has two functions makeCacheMatrix & cacheSolve that help to find inverse of
## a matrix and cache the solution so that if it is already solved return that
## else inverse the matrix and caches the result

## Write a short comment describing this function

## makeCacheMatrix function creates the Matrix object that is used for cachincg
## the inverse and use it for speeding up the inverse solution 

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  # set() function to set the special matrix
  
  set <- function (y) {
    x <<- y
    mat <<- NULL
  }
  
  # get() function to get the special matrix that is solved and cached
  
  get <- function() x
  
  # setInv() function that sets the inverse of the given matrix
  
  setInv <- function (matInv) mat <<- matInv
 
  # getInv() function that gets the inverse of the given matrix
  
  getInv <- function() mat
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## Write a short comment describing this function
## cacheSolve function used to cache the inverse matrix and return that solution if
## already cached. This helps to reduce computation and speed up the results.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check if the matrix inverse is already available after getting it
  # if it is not null return the cached matrix inverse
  
  mat <- x$getInv()
  
  if (!is.null(mat)){
    message ("getting cached data")
    return(mat)
  }

  data.M <- x$get()
  
  # call the matrix inverse solver function solve() to find the the inverse
  # cache the inverse using setInV
  
  mat <- solve(data.M, ...)
  
  x$setInv (mat)
  
  return(mat)
}
