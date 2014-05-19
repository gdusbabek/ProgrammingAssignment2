## I did not follow the pattern establisedh by the makeVector/cachemean
## examples. This implementation simplifies by pushing the cache check
## down into the 'special' matrix object.
##
## Test it ithis way:
##
## x <- matrix(rnorm(100), 10, 10)  # creates the original matrix
## xinv <- solve(x) # actual inverse of x.
## xspecial <- makeCacheMatrix(x)  # creates the special version of x
## xspecial$isCalculated() # should return FALSE because we haven't calculated the inverse
## xspecialInv <- xspecial$getInverse() # calculates the inverse.
## xspecial$isCalculated() # should return TRUE because we've calculated the inverse.
## identical(xinv, xspecialInv) # should return TRUE because they are both the inverse of x

## returns a collection of functions 
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  # handy way to test of the inverse is already calculated.
  # used for testing.
  isCalculated <- function() {
    !is.null(cachedInverse)
  }
  
  ## do the 'solve' part here. if the cache is NULL compute the cache,
  ## then return the case.
  getInverse <- function() {
    if (is.null(cachedInverse)) {
      cachedInverse <<- solve(x)
    }
    return (cachedInverse)
  }
  
  # return getInverse and isCalculated
  list(getInverse = getInverse,
       isCalculated = isCalculated)
}


## fetch the inverse from the object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$getInverse()
}

