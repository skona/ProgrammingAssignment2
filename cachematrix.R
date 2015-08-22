## Purpose of the script is to optimize matrix inversion calculation by caching the results
## so that subsequent calls to the same matrix inversion is returned from the memory cache
## The script contains two fuctions
## 1. makeCacheMatrix() Sets the inverted matrix into the cache
## 2. CacheSolve() - Caclculates the inverse of a matrix

## makeCacheMatrix() - Sets the inverted matrix into the cache
## Contains get() set() methods for the matrix and inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Create inverted matrix variable and set it to NULL
  cache <- NULL
  ## Retun the created matrix
  getMatrix <- function() {
    x
  }

  ## Set the created matrix
  setMatrix <- function(y) {
    x  <<- y
    cache <<-NULL
  }
  ## Set the inverted matrix to the cache variable and persist in memory
  setInverse <- function(solve) {
    cache <<- solve
  }
  ## Return the inverted matrix from memory
  getInverse <- function () {
    cache
  }
  ## Return the list of methods in this function
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse=getInverse)
}

## CacheSolve() - Caclculates the inverse of a matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'      
  matrixInverse <- x$getInverse()

  ## If inverted matrix is in cache then return it. DO NOT calculate again
    if(!is.null(matrixInverse)) {
    message("getting cached Matrix Inverse")  
    return(matrixInverse)
    }
  
  ## The following section executes only if the matrix inversion was called the very first time.
  ## Subsequent calls to the same matrix should skip this section of code.
  ## Get the matrix to be inverted
  data <- x$getMatrix()
  ## Invert the matrix using R solve() function
  matrixInverse <- solve(data,...)
  
  ## Set the inveted matrix in memory
  x$setInverse(matrixInverse)
  message("getting uncached Matrix Inverse")
  ## Return inverted matrix. NOT from cache
  matrixInverse
}
