## The function "makeCacheMatix" creates a matrix that caches it's inverse


makeCacheMatrix <- function(x = matrix()) {         # function takes a matrix
  m <- NULL
  set <- function(y) {                                      
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve           # takes matrix and solves for inverse, then sets new matrix
  getsolve <- function() m                          
  list(set = set, get = get,                        # create list of functions for caching matrix
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function "cacheSolve" checks for the inverse of a matrix. If not cached, calculates it.

cacheSolve <- function(x=matrix(), ...) {  # Returns a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {                        # checks to see if matrix data is cached
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m                                        # Returns inverse of matrix object 
}
