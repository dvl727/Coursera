### The makeCacheMatrix function takes an input matrix and solves
### its inverse. It also sets m to null to invalidate the cache.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
## Get returns x
get<-function() x


setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

#### CacheSolved function computes the inverse of a provided matrix, x.
### the function checks if 'm' is already computed, it returns it from 
### Cache. Otherwise, it recomputes the inverse and sets the value to m
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    datos<-x$get()
    m<-solve(datos, ...)
    x$setmatrix(m)
    m
}
