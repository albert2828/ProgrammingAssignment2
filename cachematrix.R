## The following functions are designed to store information about an invertible matrix, to 
## compute its inverse and if its inverse is already computed, to get it from the cache.

## Theese two functions are based on the examples provided by professor Peng in the README.MD file.

## This function creates a special "matrix" out of a given matrix. It creates a list that contains 
## the given matrix and is inverse. (It is recomendable to store the result in a variable.)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
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



## This functions takes a list that contains an invertible matrix and checks cache to see if the
## inverse of the matrix has been computed, if it is already computed, it recovers it from the cache,
## if not, then it computes it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
