## The functions below can be used to cache the inverse of a matrix.
## The function stores the inverse of the given matrix as cache
## So that it can be used again without having to compute again
## the <<- operator can be used to assign a value to an object in a different environment

## Function “makeCacheMatrix” creates a “matrix” object that can cache its inverse.
## makeCacheMatrix consists of 4 functions: set, get, setmean, getmean which get returned in a list

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {                         ## stores the given matrix and initialises the variable m to be used in different envirnoment
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve    ## computes inverse by using solve function
      getinverse <- function() m                   ## stores the value m      
      list(set = set, get = get,                   ## creates a list with the four functions      
           setinverse = setinverse,
           getinverse = getinverse)
}

## Function cacheSolve computes the inverse of the given matrix
## If it has been already computed, then it retrieves the inverse from the cache returned by makeCacheMatrix above
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse 
## x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()                          ## gets the value of m from cache
      if(!is.null(m)) {                            ## checks if inverse is already calculated and if yes, returns the value of m 
            message("getting cached data")
            return(m)
      }
      data <- x$get()                              ## gets the given matrix and assigns to variable data
      m <- solve(data, ...)                        ## computes the value of m and stores it in setinverse in the list 
      x$setinverse(m)
      m
}
