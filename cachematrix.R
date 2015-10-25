##To create a function namely makeCacheMatrix
makeCacheMatrix <- function(k = matrix()) {
  s <- NULL
  set <- function(y) {
    k <<- y
    s <<- NULL
  }
  get <- function() k
  setsol <- function(solve) s <<- solve
  getsol <- function() s
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol) 
}


##Use cacheSolve to compute the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(k, ...) {
  ## Return a matrix that is the inverse of 'k'
  s <- k$getsol()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- k$get()
  s <- solve(data, ...)
  k$setsol(s)
  s
}
