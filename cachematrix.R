## This set of functions inverts a matrix
## also taking care that the value gets stored in the cache 
##(by the use of superAssignment operator), so that
## in case that the matrix was perviously inverted
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


## Function checks if inverted matrix  already exists in cache
## If exists, it is returned. If it doesn't, it gets set and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m) #WTF ???
     m
}
