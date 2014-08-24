## This pair of functions allows to:
## 1st construct a special matrix that store the matrix
##     and a cached inverse of the matrix. "makeCacheMatrix"
## 2nd Calculate the inverse of the matrix if it is not 
##     previously calculate or use the cached inverse 
##     if it is previous stored. "cacheSolve"


# This function creates a special type of matrix
# that stores a cached inverse, so do not calculate every time it is needed.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                               # Set the inverse NULL for a new matrix
  set <- function(y) {                      # Creates a function that set a matrix in x
    x <<- y                                 # Use this in case you want to change the 
    inv <<- NULL                            # matrix in futher use.
  }
  get <- function() as.matrix(x)            # Creates a function that gets the stored matrix
  setinv <- function(inverse) inv <<- inverse # Creates a function that set the inverse to the "inv" cache
  getinv <- function() inv                  # Creates a function that gets the inverse from "inv"
  list(set = set, get = get,                # Retuns a list of functions with the
       setinv = setinv,                     # data of the matrix in "x".
       getinv = getinv)
  }


## This function calculate o retrieve the inverse of a matrix.
## If it is not previous calculated, it calculates and stores,
## but if there is a previous inverse just use it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                       # Stores the inverse to the "inv" local variable
                                            # calling the getinv() internal function of the
                                            # special matrix
    if(!is.null(inv)) {                     # If it is already stored then returns the                                   
      message("getting cached inv matrix")  # already calculated inverse of the matrix.
      return(inv)
    }
    data <- x$get()                         # If inv is NULL then retrive the data
                                            # with the function get()
    inv <- solve(data, ...)                 # Then calculate the inverse
    x$setinv(inv)                           # stores in the inv as cache for futher use.
    inv                                     # Then return the inv variable
  }        

