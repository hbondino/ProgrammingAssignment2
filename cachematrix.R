## This functions compute the inverse of the special "matrix"
## unless the inverse has already been calculated, in that case
## retrieves the inverse from the cache

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                INV <- NULL
                
                ## Setting the value of the matrix
                setMatrix <- function(y) {
                  x <<- y
                  INV <<- NULL
                }
                
                ## Getting the value of the matrix
                getMatrix <- function() x
                
                ## Setting the inverse of the matrix
                setinverseMatrix <- function(inverse) INV<<- inverse

                ## Getting the inverse of the matrix
                getinverseMatrix <- function() INV
                
                ## List of the methods used
                list(setMatrix = setMatrix,
                     getMatrix = getMatrix,
                     setinverseMatrix = setinverseMatrix,
                     getinverseMatrix = getinverseMatrix)
}

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                ## Getting the cached value
                INV <- x$getinverseMatrix()
                
                ## If the cached value is not null
                ## this will return the cached value
                if(!is.null(INV)) {
                  message("getting cached data")
                  return(INV)
                }
                
                ## If the cached value is null
                ## this will calculate the inversed matrix
                data <- x$getMatrix()
                INV <- solve(data, ...)
                x$setinverseMatrix(INV)
                INV
}
