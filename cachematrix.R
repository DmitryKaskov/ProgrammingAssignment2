## This pair of functions calculates inversion for a matrix passed as an argument. 
## As this is quite costly computation, it caches the result and 
## return from cache if it is needed to calculate inverse for the same matrix again
  

## This function creates a list with 4 functions to set/get the matrix and 
## set/get the inverse for that matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse for the matrix set in MakeCacheMatrix,
## but checks first if inverse  was already calculated for that matrix and
## if yes - retrieve it from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
