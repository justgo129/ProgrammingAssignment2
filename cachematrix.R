

##This function creates the inverse of a matrix and caches the inverse.

   makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
          x <<- y
          i <<- NULL
       }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


   # Computes the inverse of the aforementioned matrix assuming the matrix
   # has not yet been determined or changed

   cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
