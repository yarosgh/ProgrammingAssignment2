## Caching the Inverse of a Matrix
##  uses solve() to calculate the inverse
##  of a given matrix and by taking advantage of
##  R's scoping rules is able to cache it's results. 
##
##    >  mymat <- matrix(c(1,0,5,2), nrow=2, ncol=2)
##    > mcm <- makeCacheMatrix(mymat)
##    > cacheSolve(mcm)
##    .... outputs inverse ...
##    > cacheSolve(mcm)
##    .... outputs inverse from cache ...

## Creates a special "Matrix" which is really a list
##  contains a function to:

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  ### 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ### 2. get the value of the matrix
  get <- function() x
  ### 3. set the value of the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  ### 4. get the value of the inverse of the matrix
  getsolve <- function() m
  # return  list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Given a makeCacheMatrix object, returns the inverse 
##   of the matrix
## !!!! cacheSolve(x, ...) - where 'x' is a
##         'makeCacheMatrix' object !!!
##  (if it doesn't exist in cache it calculates it
##   and caches before returning)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # retrieve from cache
  
  if(!is.null(m)) { # we got something...
    message("getting cached data")
    return(m)
  
  }
  # cache isn't filled yet
  data <- x$get()       # get it
  m <- solve(data, ...) # solve it
  x$setsolve(m)	      # set it
  m		      # return it
}

## just as in the example
