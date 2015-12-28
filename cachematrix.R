# Create a list of functions to set and get the value of the matrix and its inverse
makeCacheMatrix <- function(x=numeric()) {
  inv <- NULL 
  set<- function(y) { # Set a value of the matrix
    x<<-y
    inv <<- NULL
  }
  get <- function() x # Get the value of the matrix
  setInv <- function(inverse) inv <<- inverse # Set the value of the inverse of the matrix and puts it in cache
  getInv<- function () inv # Get the value of the inverse of the matrix
  list (set=set, get=get, setInv=setInv, getInv=getInv)
}

# Return the inverse of x
cacheSolve <- function(x, ...) { 
  inv <- x$getInv() # Check if the inverse was already created

  if(!is.null(inv)) { # If the inverse was already created, return message to user 
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # Call the function to get the value of the matrix and assign it
  inv <- solve(data, ...) # Get the inverse of the matrix
  x$setInv(inv) # Call the function to put the result in cache
  inv #Print result
}

