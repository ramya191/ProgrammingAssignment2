## makeCacheMatrix: stores and returns getters and setters for a matrix
## and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## cacheSolve: takes a "cacheMatrix" and optional params
## and returns a stored solve result from the cacheMatrix or 
## calculates a result if not stored (result of solve)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached inverse...")
    return(inv)
  }  
  message("calculating matrix inverse...")
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}

source("ProgrammingAssignment2/cachematrix.R")
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

## Example
## A <- matrix(c(2,4,3,1,5,7,6,8,9), nrow=3, ncol=3, byrow=TRUE)
## a <- makeCacheMatrix(A)
## a$get()
##      [,1] [,2] [,3]
## [1,]    2    4    3
## [2,]    1    5    7
## [3,]    6    8    9

## cacheSolve(a)
## calculating matrix inverse...
##       [,1]       [,2]       [,3]
## [1,] -0.25 -0.2727273  0.2954545
## [2,]  0.75  0.0000000 -0.2500000
## [3,] -0.50  0.1818182  0.1363636

## a$getinv()
##       [,1]       [,2]       [,3]
## [1,] -0.25 -0.2727273  0.2954545
## [2,]  0.75  0.0000000 -0.2500000
## [3,] -0.50  0.1818182  0.1363636

## B <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
## a$set(B)
## a$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## a$setinv(solve(B))
## a$getinv()
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

