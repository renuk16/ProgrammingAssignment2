## The functions below provide the utility of calculating the inverse of a matrix and 
## cache the result and can be retrived later. 

## makeCacheMatrix creates a list of 4 functions - 1. Set the matrix, 2. Get the matrix
## 3. set the inverse 4. get the inverse

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

## cacheSolve returns the cached inverse if the matrix has not changed. Otherwise, it
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
