## These 2 functions are used in conjunction to implement the caching
## of the matrix inversion operation.
## makeCacheMatrix builds an object that can cache the result of the matrix inversion
## cacheSolve returns the cached value if available, otherwise it inverts the matrix

## Build an object to store the cached inverse matrix
## Add getters and setters for the input data and the cached inverse matrix
## @input: a matrix
## @output: the object (in R terms, a list of methods)

makeCacheMatrix <- function(x = matrix()) {
     cached_inverse <- NULL  #the stored inverse matrix
     
     #getter and setter for the input parameter
     set <- function(y) 
     {
          x <<- y
          cached_inverse <<- NULL
     }
     get <- function() x
     
     # getter and setter methods for cached value
     set_inverse <- function(inverse) cached_inverse <<- inverse
     get_inverse <- function() cached_inverse
     
     list(set = set, 
          get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}


## Invert a matrix for an object built with makeCacheMatrix
## Return the cached value if available, otherwise invert the matrix
## @input: an object built with makeCacheMatrix
## @output: the inverted matrix

cacheSolve <- function(x, ...) {
   
     # Try to get the cached value
     inv <- x$get_inverse()
     if(!is.null(inv)) 
     {
          message("getting cached data")
          return(inv)
     }
     
     # Cached value not available, invert the matrix
     data <- x$get()
     inv <- solve(data, ...)
     x$set_inverse(inv)
     
     inv
}
