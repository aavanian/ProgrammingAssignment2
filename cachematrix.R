## makeCacheMatrix and cacheSolve enables to avoid repeating the same computation (inverting
## a matrix) if it has already been done.
## To use it, assuming you have an object myMatrix of matrix type,
##      - create a cached object in the following way: myMatrix_c <- makeCacheMatrix(myMatrix)
##      - use cacheSolve(myMatrix_c) instead of Solve(myMatrix)
## Warning: do not use directly the internal functions of myMatrix_c at the risk of losing
## consistency between the embedded matrix and the cache


## makeCacheMatrix takes a matrix as argument and return an extended matrix object
## this object has four methods: set, get, setinverse and getinverse to respectively:
##      - assign a matrix to the object
##      - retrieve the matrix associated with the object
##      - assign the cached inverse of the matrix
##      - retrieve the cached inverse of the matrix
## these functions functions should not be called directly but use cacheSolve instead after
## initializing the object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will test if an cached inverse is already existing. If that is the case, it will
## return the cached value, if it isn't, it will compute it (assuming it does exist), assign it
## to the cache variable and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}