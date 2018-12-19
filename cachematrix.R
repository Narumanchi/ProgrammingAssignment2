## Put comments here that give an overall description of what your
## functions do

## function cahes the inverse of the matrix, It captures the following:
# set matrix
#get matrix
#set inverse
#get inverse

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
## This function gives the inverse of the matrix, if inverse is available in cache it returns it or calculates inverse and returns it.

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
