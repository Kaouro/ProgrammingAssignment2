## This set of functions extend the functionality of the native R matrix object 
## by implementing a simple caching mechanism for the inverse of the matrix.


# makeCacheMatrix creates an object that contains a matrix representation and its inverse
# both variables can be accessed via the get/set or getInverse/setInverse respectively.
# The inverse of the matrix is reset when a new value for the matrix is set.s
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function takes an object that was created with the makeCacheMatrix function
# it checks if the inverse of contained matrix has already been calculated.
# If yes, it returns that value. Otherwise it calculates the inverse and returns it
cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
