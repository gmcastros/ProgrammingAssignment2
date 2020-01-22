## Put comments here that give an overall description of what your
## functions do

## Example
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Write a short comment describing this function
## Here we need to store or cache the matrix
## This works with square matrixes.

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize the variable like the example above
  m <- NULL
  # create the function, is this anonymous?
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  #use the inverse function to change the matrix.
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
## Get the matrix from cache.
## Use solve, see professor's comments about this.
## If the inverse has already been calculated (and the matrix has not changed)
##, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #m <- mean(data, ...)
  m <- solve(data,...)
  x$setinverse(m)
  m  
  
}
