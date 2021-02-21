## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
creating cache matrix

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-Null
  set<-function(y){
        x<<-y
        inv<<-Null
  }
  get<-function()x
  setinv<-function(inv)inv<<-inv
  getinv<-function(){
          inv<-ginv(x)
          inver%*%x
          }
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}
 


## Write a short comment describing this function. gets cache data 

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cache data!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}




