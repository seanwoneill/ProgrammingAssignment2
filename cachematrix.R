# The functions below determine the inverse of a supplied
# matrix if this inverse matrix has not already been
# calcualted.  If already calculated, the inverse matrix
# is pulled from memory.


# makeCacheMatrix creates a special "matrix", which
# is really a list containing a function to: (1) set 
# the value of the matrix, (2) get the value of the 
# matrix, (3) set the value of the inverse matrix, 
# and (4) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #set the matrix to be inverted, using the format
  #  object$set(x).
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get the matrix set using 'set'.  Called by cacheSolve.
  get <- function() x
  
  #cache the inverse of matrix x. Called by cacheSolve.
  setinverse <- function(inverse) i <<- inverse
  
  #call the cached matrix inverse. Called by cacheSolve.
  getinverse <- function() i
  
  #List object returned upon calling makeCacheMatrix.
  #creates special "matrix" object for use with cacheSolve.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve calculates the inverse of the special "matrix" 
# created with makeCacheMatrix. However, it first checks to 
# see if the inverse has already been calculated. If so, it 
# gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  ##The object x passed to cacheSolve is the speacial
  ##"matrix" object created with makeCacheMatrix.
  i <- x$getinverse()
  
  #Check if the inverse matrix already exists
  if(!is.null(i)) {
    message("getting cached data")
    #Return preexisting inverse if available
    return(i)
  }
  
  #Else, get the matrix and calculate its
  #inverse using 'solve'
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  #Return the inverse matrix
  i
}
