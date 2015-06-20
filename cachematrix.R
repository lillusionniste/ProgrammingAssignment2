## So what I did here is basically creating two functions that  
## cache the inverse of a matrix and then retrieve the inverse  
## from the cache if the inverse has already been calculated 
## (and the matrix has not changed).

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

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


## The function part is over and below is an example
matrixSample = matrix(c(1,0,1,1,1,0,1,1,1),nrow = 3)
cachedInverse = makeCacheMatrix(matrixSample)
## The first time the cachesolve function is called 
## will calculate the inverse matrix
cacheSolve(cachedInverse)
## This time a message will appear and will return the 
## cached inverse matrix
cacheSolve(cachedInverse)
