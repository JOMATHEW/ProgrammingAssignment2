## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# The Cache variable is set as NULL 
  cache <- NULL
# The function set is creating a matrix
  set <- function(y)
    {
    x <<- y
    cache <<- NULL
}
#Obtaining the value of the matrix 
get <- function(x) 
# This inverts the matrix and stores the inverse
setMatrix <- function(inverse) cache <<- inverse
#Obtains the inverted matrix
getInverse <- function() cache
#returns the function
list(set = set, get = get,
     setMatrix = setMatrix,
     getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if (!is.null(cache)) {
    message("cached data")
#displays the matrix 
    return(cache)
  }
#Since the matrix doesnt exist. Creats a matrix
  matrix <- x$get()
  tryCatch( {cache <- solve(matrix, ...)
}
error = function(e) {
  message("Error!!!!")
  message(e)
  
  return(NA)
}
warning = function(e) {
  message("Warning!!!!")
  message(e)
  
  return(NA)
  
}
finally = {   x$setMatrix(cache)
}
#displays the final matrix
return (cache)
