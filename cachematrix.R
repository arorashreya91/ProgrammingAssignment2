##If the contents of a very long matrix are not changing and one has to caculate the inverse of this matrix
##you can save time by calculating the inverse and then caching it.
##So that everytime instead of computing the matrix, you can take it cached value, thereby saving on time
makeCacheMatrix <- function(x = matrix()) {  ##a matrix object is created whose inverse has to be cached
  m <- NULL
  set <- function(y) {      ##set the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x       ##get the matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {   ##this calculates the inverse of the above entered matrix
  m <- x$getinverse()              ##It first checks if inverse has already been calculated.
  if(!is.null(m)) {                ##If so, it takes the value from the cache without wasting time on recalculating it
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)                  ## Return a matrix that is the inverse of 'x'
  m
}














