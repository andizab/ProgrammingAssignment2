## Functions that Cache the Inverse of a Matrix
## 

## The first function, makeCacheMatrix creates a a list containing methods to
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m_inverse <<- inverse
  getInverse <- function() m_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the matrix returned by the 
## function makeCacheMatrix above. If the inverse has been calculated (and the
## matrix remains the same), then the cacheSolve will retrieve the inverse from
## the cache. (Assumption: the matrix in this practice is always invertible.)

cacheSolve <- function(x, ...) {
  m_inverse <- x$getInverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setInverse(m_inverse)
  m_inverse ## Return a matrix that is the inverse of 'x'
}
