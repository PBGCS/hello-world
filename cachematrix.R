## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
  {
## Initialize the inverse property
  invm <- NULL
  set<- function(y)
  {
    x <<- y
    inv <<- NULL
    
  }
## Method the get the matrix  
  get <- function ()x
    
## Way to set the inverse of the matrix
  setinverse <- function(inverse) invm <<- inverse
    
## Way to get the inverse of the matrix    
  getinverse <- function() invm
  
## Back a list of the methods    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  }


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
## Return a matrix that is the inverse of 'x'
    invm <- x$getinverse()
  if(!is.null(invm))
  {
    message("getting cached data")
    return (invm)
    
  }
  
  data <- x$get()
## Compute the inverse via matrix multiplication  
  invm <- solve(data)
  x$setinverse(invm)
  invm
  
}
