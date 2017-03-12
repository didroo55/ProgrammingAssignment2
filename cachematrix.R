## The program contains two major functions
## makeCacheMatrix - Contains a set of getters and setters for the matrix and the corresponding inverse
## cacheSolve - calculates inverse of a matrix only when the inverse is not calculated already 

## This function is used to cache the inverse of matrices. The function defines four other
## functions as described  below
## get ---> used to get thevale of the supplied matrix
##set ---> used to set assign value to the supplies matrix
##getinverse -->get the cache value of inverse
##setinverse --> used to set(cache) value to inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Initializing inverse value to Null
  inv <- NULL
  
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Get value of the matrix
  get <- function() {
    x
  }
    
  #Set value of inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  #Get value of inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function caches the matrix inverse calculation by calculating inverse only if the
##inverse was not calculated already

cacheSolve <- function(x, ...) {
  ## Returns the catched value of inverse (if any)
  inv <- x$getInverse()
  
  if (is.null(inv)) {  ##checks if the inverse was calculated already to cache
    ##inverse not avaiable to cache
    mat <- x$get()
    #Calculate inverse ofr the first time
    inv <- solve(mat, ...)
    x$setInverse(inv)
    return(inv)
  }
  else{ ##Inverse alreay available. Get cache data 
    message("getting cached data")
    return(inv)
  }
  
}
