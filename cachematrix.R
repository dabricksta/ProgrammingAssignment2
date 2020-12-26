##Pipes are useful
require(magrittr)
##Functions to cache potentially time-consuming matrix inversions

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
        # inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
        # the inverse from the cache.
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

####Testing####
#Create random matrix of dimensions x*x, elements between 1-10
x <- sample(1:20, 1)
dv <- sample(1:10, x^2, replace = TRUE)
mx <- matrix(dv, nrow = x, ncol = x)
#Use functions to cache inverse, then test if inverse is calculated correctly
inv <- cacheSolve(makeCacheMatrix(mx))
id <- mx %*% inv %>%
  round(.)
if (all(id==diag(x))){
  print("RETURNS EXPECTED INVERSE")
} else {
  print("FUNCTION ERROR: DOES NOT RETURN EXPECTED INVERSE")
}
message("Feel free to rerun testing code to produce additional cases")

