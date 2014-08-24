## This pair of functions allows to:
## 1st construct a special matrix that store the matrix
##     and a cached inverse of the matrix. "makeCacheMatrix"
## 2nd Calculate the inverse of the matrix if it is not 
##     previously calculate or use the cached inverse 
##     if it is previous stored. "cacheSolve"


# This function creates a special type of matrix
# that stores a cached inverse, so do not calculate every time is needed.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() as.matrix(x)
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }


## This function calculate o retrieve the inverse of a matrix.
## If it is not previous calculated, it calculates and stores,
## but if there is a previous inverse just use it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inv matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }        ## Return a matrix that is the inverse of 'x'

