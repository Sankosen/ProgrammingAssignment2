
##This function creates a global "matrix", and a list of methods which will
## set the matrix, get the matrix, sets and gets the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                            #Receives a matrix and superassigns it to x.x is a global variable now.  
    x <<- y                                       #Assigns Null to m (a global variable now).
    m <<- NULL
  }
  get <- function() x                             # get will return the matrix    
  setinverse <- function(solve) m <<- solve       # The inverse of the given matrix can be assigned to m with this method.      
  getinverse <- function() m                      # The value of m will be returned when this method is called.
  list(set = set, get = get,                      # A list of the methods defined is created to that we can call them individually. .
       setinverse = setinverse,                     
       getinverse = getinverse)

}


## This function is used to return a matrix that is the inverse of 'x' which is calculated and cached the 
## first time it is called and thereafter returned from the cached data with an appropriate message

cacheSolve <- function(x, ...) {
        
  
  m <- x$getinverse()                    # First, the inverse matrix is tried to be retrieved.
  if(!is.null(m)) {                      # If the inverse matrix is available in Cache, the message "getting cached data " 
    message("getting cached data")       # is displayed and the inverse matrix is returned.  
    return(m)
  }
  data <- x$get()                        # The cached matrix is retreived and assigned to data 
  m <- solve(data, ...)                  # As the inverse matrix is not available, it is calculated. 
  x$setinverse(m)                        # and cached with 'setinverse' and displayed.
  m
}
