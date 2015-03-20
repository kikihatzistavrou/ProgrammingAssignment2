## In this .R file, the inverse matrix from a cached object is returned.

## creates a "special" metrix object that can create the input
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL #set m to NULL
      y<-NULL #set y to NULL
      
      #set the matrix
      setmatrix <- function(y) {
            x <<- y #caches the inputed matrix
            inv <<- NULL #set m to NULL
      }
      
      #get the matrix
      getmatrix <- function() x
      
      #calculate the inverse matrix
      setinverse <- function(solve) inv <<- solve
      
      #get the invese matrix 
      getinverse <- function() inv
      
      #creates a list to store the four functions
      list(setmatrix = setmatrix, getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## If inverse matrix has been calculated this function retrieves
## the inverse matrix from cache.
## Otherwise, the fanction calculates the inverse of the new
## input matrix.solve

cacheSolve <- function(x, ...) {
      
      #gets the invese of a matrix if it is already calculated
      inv <- x$getinverse()
      
      #check if function cacheSolve has used before
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # if not takes the input matrix
      data <- x$getmatrix()
      
      #cache input matrix
      #x$setmatrix(data) 
      
      #calculates the invese matrix
      inv <- solve(data, ...)
      
      #cache the inverse
      x$setinverse(inv)
      
      #return the inverse matrix
      inv
}
