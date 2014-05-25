## Put comments here that give an overall description of what your
## functions do

## this function gets simulates OO and creates 4 methods , 
## get , setInverse , getInverse and set

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <-function(y) {  
            x<<-y #updates value of X
            inverse<<- NULL
      }
      get <- function() x # returns value of x
      setInverse<- function(solve) inverse <<- solve #set the inverse with solve
      getInverse <- function() inverse #returns inverse of x 
      #returns a list of the methods that u can use without this function
      list (set = set, 
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
      #if there is a cached inverse "!=NULL" then dont compute it
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      #if not , compute it with solve and cache the result with setInverse
      matrix <- x$get()
      inverse <- solve(matrix)
      x$setInverse(inverse)
      inverse  
}
