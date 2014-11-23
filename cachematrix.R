## This set of functions inverts a matrix
## also taking care that the iverted matrix gets stored in the cache 
## so that in case that the matrix was perviously inverted
## the inversion process doesn't have to be executed again
############################################################
############################################################
############################################################
############################################################
############################################################
## Function defines set of inner functions (as elements of the list)
## which are necessary to provide operations required to do 
##the matrix inversion (getting the parameters, setting them etc.)
##excluding making the inversion itself, which is done in the other function

makeCacheMatrix <- function(x = matrix()) {
     
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          getinv = getinv,
          setinv = setinv
          )

}


## Function checks if inverted matrix (of matrix currently passed as
## a parameter of the function) already exists in cache
## If exists, it is returned. If it doesn't, it is set and returned

cacheSolve <- function(x, ...) {
        
     
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
