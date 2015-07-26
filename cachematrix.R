## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## what if matrix changed?
      
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      inv
}


## Test run:

##> matrix <- matrix(c(1,1,1,0), nrow = 2, ncol = 2)
## > matrix
##       [,1] [,2]
## [1,]    1    1
## [2,]    1    0

## > cachedMatrix <- makeCacheMatrix(matrix)

## No cached inverse exists during the first run
## > cacheSolve(cachedMatrix)
##       [,1] [,2]
## [1,]    0    1
## [2,]    1   -1

## Cached inverse exists during the second run
## > cacheSolve(cachedMatrix)
## getting cached inverse
##       [,1] [,2]
## [1,]    0    1
## [2,]    1   -1