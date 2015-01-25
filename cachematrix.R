## Put comments here that give an overall description of what your
## functions do

## This function makes a special type of obect that allows a matrix to be stored.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ##Set the value of the matrix in environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Allow value of matrix to be retrieved.
  get <- function() x
  
  ##Add the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ##Ability to pull the inverse of the matrix
  getinverse <- function() m 
  
  ##Creates a list of of functions to manipulate the matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This funtion checks to see if the inverse of the matix has already been stored and if not ceates and stores.

cacheSolve <- function(x, ...) { 
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Pulls the getinverse value from supplied matrix.
  m <- x$getinverse()
  
  ##If the inverse already exists then it will print statement and exit the function with the value
  if(!is.null(m)) {
    message("getting cached data")
    
    ##End function and return M
    return(m)
  }
  
  ##Get the value of the matrix
  data <- x$get()
  
  ## Invert the matrix
  m <- solve(data, ...)
  
  ##Assign the inversion value
  x$setinverse(m)
  
  ##Return m value
  m
}
