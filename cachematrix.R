## Put comments here that give an overall description of what your
## functions do

## This function creates a "special" matrix which is really a list containing a function to
##    1.  Set the value of the matrix
##    2.  Get the value of the matrix.
##    3.  Set the inverse of the matrix.
##    4.  Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function (inverse) i <<- inverse
  getInverse <- function () i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the "special" matrix returned by makeCacheMatrix.  If the inverse has
## already been calculated theis function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        i <- x$getInverse()
        if (!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)
        i
}
