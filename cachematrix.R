## CACHING THE INVERSE OF A MATRIX

## The first function, makeCacheMatrix, creates a special "matrix" object that 
## contains a function to cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## The following second function, cacheSolve, returns the inverse of the matrix 
## created in the first function (see above). However, it first checks wether the 
## inverse has already been calculated. If so, then the cachesolve function will 
## get the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the inverse in the cache via the 
## setmatrix function.

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }      
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
}