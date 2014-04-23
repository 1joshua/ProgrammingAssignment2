## receives a matrix and checks if the inverse is cached
## if cached it returns the cached inverse
## if not cached it computes the inverse, caches it, then returns it

## create the matrix and corresponding functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  m <- x$getInverse()        
  if(!is.null(m)) {     
    message("getting cached data") 
    return(m)                
  }
  data <- x$get()
  m <- solve(x$get())
  x$setInverse(m) 
  m                       
}
