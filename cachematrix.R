## These functions cashe inverse matrix 
## The function makeCasheMatrix operates with cashe when 
## the function casheSolve calculates the inverse matrix 
## If the inverse matrix has already been calculated then the fucntion retrieves it from the cache.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # We set the value of the matrix and clear the previous inverse from the cache
  set <- function(y) {
    x <<- y    
    m <<- NULL 
  }
  
  # We get the value of the matrix
  get <- function() x
  
  # We set the inverse. 
  setInverse <- function(inverse) m <<- inverse
  
  # We get the inverse. 
  getInverse <- function() m
  
  # We return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## It returns inverse of matrix x
## 
cacheSolve <- function(x, ...) {
  
  # We get the cached value for the inverse
  m <- x$getInverse() 
  # If the cache is not empty, we return it
  if(!is.null(m)) {  
    message("getting cached data")
    return(m)
  }
  
  # If cache is empty We calculate inverse matrix, cache it, and return it.
  data <- x$get()  # Get value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}
