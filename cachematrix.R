#This function creates a matrix object that can cache its inverse
#The last line of makeCacheMatrix function creates a list of the four functions and returns it. 
#This list allows access to the matrix and its inverse and to modify them.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve function checks if the inverse is already cached
#If yes, it returns the cached inverse
#If not, it retrieves the current matrix, computes its inverse, caches it and returns it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}