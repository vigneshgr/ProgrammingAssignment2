## Assignment 2 - Creating a pair of functions to cache the inverse 
## of a matrix

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y ## Making a copy of the matrix to retain
    m<<-NULL ## setting a default value to track whether matrix was inverted
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix) ## returns a list of 4 values that will be used for caching
  
}


## Showing the cached results if possible

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m## Return a matrix that is the inverse of 'x'
}
