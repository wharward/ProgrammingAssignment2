## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse. 

## The first function creates a special matrix --
## Please see the line item comments to see each step of the process

makeCacheMatrix <- function(x = matrix(), ...) {
  m <- NULL  ##create a placeholder variable
  
  ## set the value of the matrix  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setsolve <- function(solve) m <<- solve
  
  ## get the value of the inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function will calculate the inverse 
## of the matrix created with the above function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  ## First, it will check to see if the inverse is already stored in the cache
  ## If so, it will get the inverse from the cache and skip the computation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise, it calculates the inverse of the matrix, and sets the value of the inverse
  ## using the getsolve function
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
