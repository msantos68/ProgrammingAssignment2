## Michelle Santos 2/9/2015
## Calculating the matrix inverse can be computational intensive.
## Functions below will calculate and cache the matrix inverse.
## If the matrix inverse is required it will determine whether or not it 
## is already cached before recomputing.

## makeCacheMatrix: This function will cache the matrix inverse into the 
## object m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMI <- function(solve) m <<- solve
  getMI <- function() m
  list(set = set, 
       get = get,
       setMI = setMI,
       getMI = getMI)
}


## cacheSolve: This function will look to see if the matrix inverse is cached
##  If not it will calculate the matrix inverse and cache.

cacheSolve <- function(x, ...) {
  m <- x$getMI()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setMI(m)
  ## Return a matrix that is the inverse of 'x'
  m
  
        
}
