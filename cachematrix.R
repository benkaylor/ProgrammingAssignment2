## cacheMatrix uses lexical scoping to cache an inverse of a matrix
## sample Call m <- matrix (c(1,2,3,4), nrow =2, ncol =2)
## mymatrix <- makeCacheMatrix(m)
## cacheSolve(mymatrix)

## makeCacheMatrix Function creates special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
      set <- function(y = matrix()) {
      x <<- y
      i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve Function computes inverse of the special "matrix" returned by makeCacheMatrix above.
# If inverse already calculated (and matrix unchanged), 
# Then cacheSolve retrieves inverse from cache.
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
