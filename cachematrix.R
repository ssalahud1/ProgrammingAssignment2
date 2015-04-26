## This program contains two functions for calculating the inverse of a matrix.
## We will be caching the calculated inverse and retrieving from cache if the data is in cache.
## This will improve the overall performance and efficiency of the code.

## makeCacheMatrix gets and sets the matrix.
## cacheSolve calculates the inverse of the input matrix.

## makeCacheMatrix function has the getter and setter functions to get data from cache and 
## to and set the inverse of a matrix to cache.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list (set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}

## cacheSolve function gets the matrix inverse if it is in cache; 
## else it calculates and saves the inverse to the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
