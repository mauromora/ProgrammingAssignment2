In some ocassions it might be worthy to construct computations able to store data that does not
change in time so repetitions of calulations are avoided and instead looked up first in the cache memory.

An example of this situation is matrix inversion, a costly computation.

Accordingly, makeCacheMatrix in this example creates a matrix that is able to store its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,setmean = setmean,getmean = getmean)
}

Furthermore, cacheSolve is a function able to compute the inverse matrix if it was previously computed
  in makeCacheMatrix, avoiding repetition of calculations.

cacheSolve <- function(x, ...){
  inv <-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

