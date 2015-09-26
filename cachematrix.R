## Two functions below
##   1) makeCacheMatrix reads in a matrix and caches it for use elsewhere
##   2) cacheSolve reads in a matrix or retrieves it from cache and then returns the inversion of it

##  reads in a matrix and caches it for use elsewhere

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) s <<- solve
  getmatrix <- function() s
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve reads in a matrix or retrieves it from cache and then returns the inversion of it

cacheSolve <- function( x , ...) {
  s <- x$getmatrix()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setmatrix(s)
  s
}
