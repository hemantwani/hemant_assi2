## makeCacheMatrix: This function creates a  "matrix" object that can cache its inverse. cacheSolve:
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse    ## set the value of the matrix
  getinverse <- function() i                       ## get the value of the matrix
  list(set = set,                               
       get = get,
       setinverse = setinverse,                    ## set the value of the inverse
       getinverse = getinverse)                    ## get the value of the inverse
}

## This function computes the inverse of the  "matrix" returned by makeCacheMatrix above
## If it is already evaluated and remain unchanged then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)                         #### inverse returned after computation
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
