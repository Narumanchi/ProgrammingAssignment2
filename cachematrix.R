## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makecachematrix <- function(x = matrix()) 
{
  i<-NULL
  set<-function(y)
  {      
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}
## Write a short comment describing this function

cachesolve<-function(x,...)
{
  i<-x$getinv()
  if(!is.null(i))
  { 
  i                ###if inverse available in cache, exits without calculating again, giving the cached value, i
  }else{
  d<-x$get()
  i<-solve(d,...) ###calculating inverse of the matrix
  x$setinv(i)
  i}
}
