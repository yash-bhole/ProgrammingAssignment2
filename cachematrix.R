## The functions below can be used to cache the inverse of a matrix.
## The function stores the inverse of the given matrix as cache
## So that it can be used again without having to compute again
## the <<- operator can be used to assign a value to an object in a different environment

## Function “makeCacheMatrix” creates a “matrix” object that can cache its inverse.
## makeCacheMatrix consists of 4 functions: set, get, setmean, getmean which get returned in a list

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

## Function cacheSolve computes the inverse of the given matrix
## If it has been already computed, then it retrieves the inverse from the cache returned by makeCacheMatrix above
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse 
## x$setmean(m) stores it in the object m in makeCacheMatrix.

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
