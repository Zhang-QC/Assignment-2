## This program allows the user to generate a matrix and then calculate the inverse of the matrix while caching each result to be reused later. 


## The function makeCacheMatrix creats a special matrix that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


# The function cacheSolve compute the inverse of the special matrix. If the inverse have been calculated and stored in a cache, then the function retrieve the value from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) 
      {
            inverse
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

matrix <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(matrix)