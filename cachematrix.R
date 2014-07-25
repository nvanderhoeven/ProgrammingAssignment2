## Put comments here that give an overall description of what your
## functions do

## This function, makeCacheMatrix, creates a list containing the options to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of what is created in the function above  However, it first checks
## whether or not the inverse is already calculated. If so, it skips the calculation and gets the previous
## computation from the cache. If not, the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_ <- solve(data)
  x$setinverse(inv_x)
  inv
}

