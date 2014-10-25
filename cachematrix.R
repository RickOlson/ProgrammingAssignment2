# The functions makeCacheMatrix and cacheSolve allow the inverse of a matrix
# calculated and cached for easier access.  
#
# makeCacheMatric(myMatrix) is used to link the matrix myMatrix to the functions 
#    needed to cache the inverse and access the cached inverse.
# cacheSolve(myMatrix) is used when the inverse of myMatrix is needed.

# Function the defines the functions needed to define a matrix,
# calculate and cache the inverse and use the cached inverse when known
makeCacheMatrix <- function(mymatrix = matrix()) {
  # minverse is the inverse of the matrix mymatrix.  It will be cachedonce calculated
  # minverse is initially NULL because mymatrix hasn't been defined
  myinverse<-NULL
  
  # Function setmatrix is used todefine the value of vector mymatrix
  setmatrix<-function(y,...) {
    
    # Set the vector mymatrix to a new value passed in as the variable y
    mymatrix<<-y
    # Since the vector has been reset, it's new and the mean hasn't been cached
    myinverse<<-NULL # Resets mean calc to Null
  }
  
  # Function getmatrix is used to return the value of mymatrix
  getmatrix<-function()mymatrix
  
  # Function setinverse receives an inverse calculated elsewhere and 
  #   caches it into the variable myinverse
  setinverse<-function(inverse)myinverse<<-inverse
  
  # Function getinverse returns the inverse if known, of NULL if the inverse hasn't been calculated
  getinverse<-function() myinverse
  
  list(setmatrix=setmatrix, getmatrix=getmatrix,
       setinverse=setinverse,
       getinverse=getinverse)
}



# Function used to retreive and store the inverse of a matrix
# A cached value is used if it exists
#
cacheSolve <- function(mymatrix, ...) {
  # Try to retreive the inverse of mymatrix
  # If the inverse has not been calculated already, NULL will be returned
  # If it has been calculated, the inverse will be returned
  myinverse <- mymatrix$getinverse()
  
  if(!is.null(myinverse)) {
    # The inverse was cached, so return it
    message("getting cached data")
    return(myinverse)
  }
  
  # The inverse wasn't cached.  Retreive the vector, then and calculate its inverse
  myinverse<-solve(mymatrix$getmatrix(),...)
  
  # Cache the calculated inverse
  mymatrix$setinverse(myinverse)
  
  # Return the inverse
  myinverse
}