## DOne by: Aditya Agrawal
##23-06-2016

##Returns a special matrix(list of functions) for the input of a matrix,
##containing functions for setting and retrieving the matrix, and inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #Functions for returning matrix, and inverse respectively.
  get <- function() x
  
  getInv <- function() inv
  
  #Function for setting the inverse.
  setInv <- function(inverse){
    inv <<- inverse
  }
  
  #List of functions is returned.
  list(set = set, get = get, getInv = getInv, setInv = setInv)
}


##Returns the inverse matrix of the matrix provided in the makeCacheMatrix function.
##Either the cached value is returned or the inverse is calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Getting cached value.")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
}
