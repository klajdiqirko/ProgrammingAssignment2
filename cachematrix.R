#  The function consists of two functions. The pair caches
# the inverse of a function after it is computed so that we
# do not have to compute it repeatedly but just get it 
# from the cache once computed.

## The first function creates a list of functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set , get = get,
       setinverse = setinverse, getinverse = getinverse)
}


# This second function computes the inverse of the matrix created above.
# It first checks if the inverse has already been computed. 
# If it was previously computed it gets the inverse from the cache 
# and returns it, otherwise it computes it and then sets the inverse 
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message('getting cached data')
          return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
