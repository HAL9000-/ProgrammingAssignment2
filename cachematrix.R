## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Set the value of the matrix
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## Get the value of the matrix
      get <- function() x
      ## Set the inverse of the matrix
      setinverse <- function(solve) m <<- solve
      ## Get the inverse of the matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      ## use cached value if already calculated and matrix hasn't changed
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## otherwise calculate inverse of matrix and return the matrix
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
