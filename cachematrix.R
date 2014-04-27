makeCacheMatrix <- function(x = matrix()) {
  # i will store the cached inverse matrix
  i <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y         
    i <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  # get the value of the inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  #calcualtes the inverse of the matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}