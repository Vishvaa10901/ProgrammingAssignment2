
## Create a function that starts with null mmatrix argument.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL ##Initialization
  
  ## Set mtrix function below to set the value of the matrix in the cache
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## Get function below to get the value of the marix
  get <- function() x
  
  ## SetInverse function below to set the inverse value of the matrix in cache
  setinverse <- function(inverse) m <<-
    inverse
  
  ## GetInverse function below to get the inverse of the matrix from cache
  getinverse <- function() m
  
  ## passes the value of the function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  to get the value of the matrix from cache if available

cacheSolve <- function(x, ...) {
  ## geting the inverse of the matrix from the cache
  m <- x$getinverse()
  
  ## Checks if it is null
  if(!is.null(m)){
    ## If found in cache-- directly print without calculating
    message("getting cached data - Inverse of the matrix")
    return(m)
  }
  
  ## If not in cache-- 
  data <- x$get()
  
  ## Calculate the Inverse of the matrix with solve() function
  m <- solve(data, ...)
  
  ## Set this inverse of the matrix to cache for future use
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

