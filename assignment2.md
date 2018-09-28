#### Caching function and function computing inverse
##### function caching inverse 

makeCacheMatrix <- function(x = matrix()) 
{
inv <- NULL
set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }                                                   
   get <- function() x
   
   ##### Setting of the data
   
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set,
   get = get,
       setInverse = setInverse,                                
       getInverse = getInverse)                       
}


 #### Function computes inverse returned by makeCacheMatrix function

cacheSolve <- function(x, ...)
{  
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
