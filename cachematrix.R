## My function is a set of functions that cache and compute
## the inverse of a matrix

## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
  inverse<-NULL
  set<-function(x){
    mx<<-x;
    inverse<<-NULL;
  }
  get<-function() return(mx);
  setinv<-function(inv) inverse<<-inv;
  getin<-function() return(inverse);
  return(list(set=set,get=get,setinv=setinv,getinv=getinv))
}


## This function computes the inverse of the "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed, then cacheSolve should retrieve the inverse
## from the cache.
cacheSolve <- function(mx, ...) {
  inverse<-mx$getinv()
  if(!is.null(inverse)){
    message("getting cache data...")
    return(inverse)
  }
  data<-mx$get()
  inverse<-solve(data,...)
  mx$setinv(inverse)
  return(inverse)
}
