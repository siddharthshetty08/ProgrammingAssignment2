## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function can be used to create an inverse of a matrix and cache it.
## It creates a list containing the functions to set and get the value of a matrix 
## and to set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <-  function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the list returned by
## makeCacheMatrix. It retrieves the inverse from the cache if the inverse
## has already been calculated (and the matrix has not changed),then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <-x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data,... )
  x$setinverse(i)
  i
}
