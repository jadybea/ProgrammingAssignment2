## This is a pair of functions that inverts a matrix and stores both the matrix and its inverted matrix in the parent environment 
## of the local environment. Whenever the cacheSolve function is employed for inversion of a matrix, it makes a search if the very
## same matrix and its inverted matrix calculated previously by the same function is not stored in the parent environment due to the 
## action of the makeCacheMatrix. If the inverted matrix is found it is returned without recalculation. If a matrix has never been 
## inverted yet, these pair of functions calculate the inverted matrix and stores the result in the parent environment of the local 
## environment.

## This function contains four functions that aim to store and search for the matrix and its inverted matrix in the parent
## environment of the local environment. 

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
            x <<- y
            i <<- NULL
     }
     get <- function() x
     setinv <- function(inv) i <<-inv
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function calculates the inverse of a matrix unless it has previously been calculated and stored. If the matrix and 
## its inverted matrix has already been calculated and stored, it simply returns the previously calculated inverted matrix.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)  
      }
      data <- x$get()
      i <- solve(data, ...)    ## Return a matrix that is the inverse of 'x'
      x$setinv(i)
      i
}
