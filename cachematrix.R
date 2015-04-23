##This Code is to find the Inverse of a Square Matrix using "makeCacheMatrix" and "cacheSolve functions"
##makeCacheMatrix is to create the matrix
##cacheSolve is to find the inverse of the above matrix

makeCacheMatrix <- function(x = numeric()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) 
    mat <<- solve
  getinverse <- function() 
    mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
