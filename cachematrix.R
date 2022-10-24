## The overall purpose of the functions is to cache the inverse of a matrix

## The first function sets the value of the matrix. 
## Thereafter it sets the value of the inverse matrix and gets said  matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}

## The second function computes the inverse of the matrix by using the "solve" command. 
## Furthermore if the inverse has already been calculated, then the cachesolve will instead retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## Testing the functions
## I will first create a simple square matrix:
example_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

example_matrix$get()

example_matrix$getsolve() ## Return the inverse of the matrix which at this point should be NULL

cacheSolve(example_matrix) ## Use cacheSolve to get the cached data

example_matrix$getsolve() ## Retrieve it directly with the same function as earlier, now that I've acquired the cached data
