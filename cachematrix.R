## Use:
## 
## >m = matrix( c(1, 1, 2,  2,1,2 ,1,-1,1),nrow=3, ncol=3) # an example (sample matrix) for test. Note: test if solve(m) is defined.
## load the funcion
## >source("cachematrix.R")
## created cached matrix:
## >mcached <- makeCacheMatrix(m)
## mcached$get() # to get 'internal' matrix 
## Then use:
## cacheSolve(mcached) #calculate solve(mcached) or get from the cache

# this function define 4 internal function, set() and get() as internal setter and getter of matrix. 
# setInverse() and getInverse() to access inverse of m 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { 
    x
  }
  setInverse <- function(solve){
    m <<- solve
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## get as input a makeCacheMatrix object, 
##  if getInverse() is null calculate the inverse and put the result into the object (x$setInverse(m))
##  otherwise get result from the cache (x$getInverse())
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
