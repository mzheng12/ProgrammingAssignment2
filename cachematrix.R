## These functions cache the inverse of a matrix

setwd("C:/Users/13024/OneDrive/Documents/Linked-RStudio")

# Creates "matrix" object that caches its inverse 
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,  get = get, setinv = setinv, getinv= getinv)
}

# Retrieves matrix inverse if it is cached, otherwise computes matrix inverse and caches it
cacheinv <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Example
# mat1 <- matrix(1:4,nrow=2,ncol=2)
# cacheinv(makeCacheMatrix(mat1))