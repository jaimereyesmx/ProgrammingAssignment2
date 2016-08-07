## Performs matrix inversion in square invertible matrices; we cache the matrix
## inverse after it is calculated to avoid unnecessary computation and improve
## performance.

## Returns a list of functions that allow  a 'matrix' object to cache its matrix 
## inverse
##    'inverse' contains the matrix inverse if already calculated
makeCacheMatrix <- function(x = matrix())
{
   inverse <- NULL
   
   set <- function(y)
   {
      x <<- y
      inverse <<- NULL
   }
   
   get <- function() x
   
   setInverse <- function(inverseResult) inverse <<- inverseResult
   
   getInverse <- function() inverse
   
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns the matrix inverse of x; by identifying whether the inverse has 
## already been calculater and computes the inverse if neccesary, otherwise
## return cached result.
cacheSolve <- function(x, ...)
{
   inverseResult <- x$getInverse()
   
   if(!is.null(inverseResult))
   {
      message("Returning cached result")
      return(inverseResult)
   }
   
   matrix <- x$get()
   inverseResult <- solve(matrix, ...)
   
   x$setInverse(inverseResult)
   
   inverseResult
}