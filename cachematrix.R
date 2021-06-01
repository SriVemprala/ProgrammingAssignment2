## Put comments here that give an overall description of what your
## functions do
## This R code contains 2 functions:
## 1) makeCacheMatrix : to create and set a matrix 
## 2) cacheSolve : checks if there already exists an inverse matrix,else will generate one

## Write a short comment describing this function
## This function creates and set a matrix
## Uses lexical scoping to set the variables 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  getmatrix <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function first checks if an inverse exists for a function.
## 
cacheSolve <- function(x, ...) {
  # if inverse  exist it will be returned in the if block  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("already inverse matrix exists in cache")
    return(inv)
  }
  # if inverse doesn't exist then it will get the matrix to solve the inverse and return
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
