## the functions below compute and store the inverse of a matrix
## when the function is called repeatedly, it can use the cached value

## This function creates a list of functions to access the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # set inverse to NULL for the first time
    inv <- NULL 
    set = function(y) {
    x <<- y
    inv <<- NULL
  }
    get <- function() x #function to get the matrix x
    setinv <- function (inverse) inv <<- inverse #function to set the value passed to 'inv' variable
    getinv <- function() inv #function to get the inverse of the matrix x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) #return a list containing the functions

}


## this function checks if inverse of matrix is stored in cache
## if not stored then it computes the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #get the inverse of the matrix x
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } #if inverse is not NULL get the cached data
  data <- x$get() #if inverse is NULL get the matrix
  inverse <- solve(data) #compute inverse of the matrix
  x$setinv(inverse) #store inverse of matrix in cache
  return (inverse)
}
