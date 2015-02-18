### The makeCacheMatrix function takes an input matrix and solves
### its inverse. It also sets m to null to invalidate the cache.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                    "Invalidates cache"
  set<-function(y){          "Set function resets x with new matrix"
  x<<-y                      
  m<<-NULL
}
## Get returns x
get<-function() x             "Get returns current matrix"


setmatrix<-function(solve) m<<- solve    "Caclulates Matrix Inverse"
getmatrix<-function() m                 "Retrieves matrix inverse"
list(set=set, get=get,                  "Creates list that is returned"
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

#### CacheSolved function computes the inverse of a provided matrix, x.
### the function checks if 'm' is already computed, it returns it from 
### Cache. Otherwise, it recomputes the inverse and sets the value to m

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()          "get m from x (returned from makeCacheMatrix"
    if(!is.null(m)){       "m is not null only when it has been calc before"
      message("getting cached inverse matrix") 
      return(m)                           "Return the mean"
    }
    
    ## Below code is only called when m is null 
    datos<-x$get()        "Gets the matrix made in makeCacheMatrix"
    m<-solve(datos, ...)  "calculates inverse of the matrix"
    x$setmatrix(m)        "Stores the inverse "
    m
}
