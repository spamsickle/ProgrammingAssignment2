## These two functions (makeCacheMatrix and cacheSolve)
## work together to encapsulate a matrix and its inverse.
## They illustrate one way to cache data which is 
## difficult or time-consuming to calculate.


## Create a "container" for a matrix, its inverse, 
## and functions for setting and returning both

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix expects to be called with a matrix
  #   ("x") as its sole argument. If none is provided, 
  #   it creates an empty matrix by default.
  
  # At this point, the matrix is stored in "x"
  m <- NULL    # m will store the inverse of the matrix.
               #   NULL when created, it will be 
               #   set later by a call to "setinverse"
  set <- function(y) { # The "set" function replaces the
                       # existing matrix in this container
                       # with a new matrix 
    x <<- y    # store the new matrix
    m <<- NULL # invalidate the old inverse
  }
  get <- function() x  # return the matrix
  setinverse <- function(solve) m <<- solve  #store inverse
  getinverse <- function() m    # return the inverse
  list(set = set, get = get,    # create (and return) a
       setinverse = setinverse, #   list of functions held 
       getinverse = getinverse) #   by this container
}


## This function returns the inverse of the matrix 
##      in the container created by makeCacheMatrix.
## If no inverse is cached, this function calculates and
##      caches it before returning it to the caller.

cacheSolve <- function(x, ...) { # "x" is the container
                           # created by makeCacheMatrix
    ## Return a matrix that is the inverse of 
    ##        the matrix contained in 'x'
    m <- x$getinverse()     # inverse already cached?
    if(!is.null(m)) {          # Yes, let the world know
      message("getting cached data")   # where we got it
      return(m)                        # and return it
    }
    data <- x$get()            # No, get the matrix
    m <- solve(data, ...)      #     calculate inverse
    x$setinverse(m)            #     cache it
    m                          #     and return it
}
