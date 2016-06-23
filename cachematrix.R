## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Returns a special matrix(list of functions) for the input of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInv <- function() inv
  setInv <- function(inverse){
    inv <<- inverse
  }
  list(set = set, get = get, getInv = getInv, setInv = setInv)
}


## Write a short comment describing this function
##Returns the inverse matrix of the matrix provided in the makeCacheMatrix function.
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
