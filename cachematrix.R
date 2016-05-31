## The following functions will cache the inverse of a matrix.  

## Creates a special "matrix" object that can cache its inverse.
## For this exercise, this function is called first with an invertable square matrix
## (per the assumption stated in the assignment).  

## Also, the assignment states that the cache version is returned if it has already been calculated
## and if the matrix has not changed.  If the matrix changes, it will call the set function to store
## the new matrix and set the new Inverse of the matrix.  Therefore, I did not do any explicit checking
## to determine whether the matrix changed in the CacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          set <- function(y) {
                    x <<- y
                    inverse <<-NULL
          }
          get <- function() x
          setInverse <- function(solveData) inverse <<- solveData
          getInverse <- function() inverse
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inverse <- x$getInverse()
          if(!is.null(inverse)) {
                    message("getting cached data")
                    return(inverse)
          }
          data <- x$get()
          inverse <- solve(data)
          x$setInverse(inverse)
          inverse
}