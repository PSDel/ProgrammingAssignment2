## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function creates list, containing functions to:
# set values of the matix
# get values of the matrix
# set the values of the inverse matrix
# get the values of the inverse matrix
#
# Matrix x supplied for this funtion needs to be invertible, if it is not, then 
# using the cachesolve function will throw an error.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#  Matrix x supplied to this dunction has to be an object created using the
#  makeCacheMatrix funtion.
# This function returns inverse matrix to the matrix in object x.
# It either computes inverse matrix (and stores it within object x), 
# if it is not already cached in the x object. If object x already conains cached
# inverse matrix, the function will simply retrieve it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  mat_values <- x$get()
  inv <- solve(mat_values)
  x$setinv(inv)
  inv
}
