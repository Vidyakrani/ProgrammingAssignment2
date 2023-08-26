## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions that interact with a stored matrix and its inverse.
## The list contains methods to set and get the matrix and its inverse.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve retrieves the inverse from the cache.
## It returns the inverse matrix.


cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

