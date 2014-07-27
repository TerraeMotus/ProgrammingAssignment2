## The following code consists of two functions which are intended, as a whole,
## to compute the inverse of a square matrix efficiently, which means that
## if the inverse of the matrix has already been computed, the value of the
## inverse will be retrieved from the cache rather than being recomputed,
## insofar as the matrix remains the same.

## The first function, 'makeCacheMatrix', is a list of functions that allows the
## user to set the matrix (and therfore get it afterwards), and cache its inverse
## once it's been calculated.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The second function, 'cacheSolve', first finds out if the inverse of the matrix
## has already been calculated: if so, it retrieves its value from the cache and
## displays it, otherwise it computes it and invokes 'makeCacheMatrix', so that
## the newly computed inverse gets stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
