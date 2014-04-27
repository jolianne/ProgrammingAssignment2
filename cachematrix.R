## makeCacheMatrix creates the vector inv.
## The inverse of the matrix x is computed, and affected to inv via setInv and getInv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<-inverse
        getInv <- function() inv
        list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## cacheSolve calculates the inverse of invMatrix created by makecacheMatrix.


cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  
  return(inv)
}
