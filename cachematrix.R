
## This functions return list representing cachable matrix
makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) r <<- inverse
  getInverse <- function() r
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function Inverses given matrix if result is not cached yet 
cacheSolve <- function(x, ...) {
  r <- x$getInverse()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data)
  x$setInverse(r)
  r
}
