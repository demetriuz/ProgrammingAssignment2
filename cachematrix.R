# Functions for caching matrix inversion results
# usage:
# x <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(x)  # first call invokes solve()
# cacheSolve(x)  # second call returns cache result

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  get <- function()
    x
  
  setInversed <- function(x)
    inversedMatrix <<- x
  getInversed <- function()
    inversedMatrix
  
  list(
    set = set,
    get = get,
    getInversed = getInversed,
    setInversed = setInversed
  )
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInversed()
  if (!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  data <- x$get()
  inversedMatrix <- solve(data)
  x$setInversed(inversedMatrix)
  inversedMatrix
}
