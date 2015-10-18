## Summary: This functions creat an object with set/get methods for 
##          a matrix data and set/get methods to cache solve operation
## Example:
##    mtx_test <- makeCacheMatrix( matrix(c(1:4), nrow = 2, ncol = 2) )
##    cacheSolve(mtx_test)

## Create a list with 4 methods to set/get matrix and cache operations

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setcache <- function(to_cache) cache <<- to_cache
  getcache <- function() cache
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## Return a matrix's solve operation and cache the result 

cacheSolve <- function(x, ...) {
  msolve <- x$getcache()
  if(!is.null(msolve)) {
    message("getting cached data")
    return(msolve)
  }
  data <- x$get()
  msolve <- solve(data, ...)
  x$setcache(msolve)
  msolve
}
