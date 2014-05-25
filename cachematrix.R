## cachematrix.R, mdponce, 25 May 2014
## Coursera, R Programming assignment #2

## These functions work together to cache the inverse of a matrix
## in order to reduce computational cost, so that the inverse is
## not repeatedly computed

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize m
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() m
  
  ## return the methods associated with this function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## assign the cached inverse to m
  m <- x$getinverse()
  
  ## should m not be null, return m (i.e, the inverse)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## in the case m is null, assign the matrix to data
  data <- x$get()
  
  ## assign the inverse of the matrix to m
  m <- solve(data, ...)
  
  ## set inverse in cache to m
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m  
}
