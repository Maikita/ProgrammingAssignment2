## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



## make cache matrix
makeCacheMatrix <- function(x=numeric()) {
  inv <- NULL
  set<- function(y) {
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv<- function () inv
  list (set=set, get=get, setInv=setInv, getInv=getInv)
}

##returns Matrix inverse of x
cacheSolve <- function(x, ...) {
  inv <- x$getInv()

  if(!is.null(inv)) { ##looks if "inv" has already been calculated and returns a message to the user
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

