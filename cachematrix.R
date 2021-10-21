## caching the inverse of matrix

## computation of inverse of a matrix is costly.

## It is therefore necessary to store the inverse and retrieve it when needed



## The function creates a special matrix whose inverese we want cache



makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    mat_inv <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(inverse) mat_inv <<- inverse
  
  getInverse <- function() mat_inv
  
  list(set = set,
       
       get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
  
}



## The function below calculates the inverse of the special matrix if not present.

## If the the inverse of the same matrix has already been calculated,

## then it just retrieves it

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  mat_inv <- x$getInverse()
  
  if(!is.null(mat_inv)) {
    
    message("getting cached data")
    
    return(mat_inv)
    
  }
  
  data <- x$get()
  
  mat_inv <- solve(data, ...)
  
  x$setInverse(mat_inv)
  
  mat_inv
  
}