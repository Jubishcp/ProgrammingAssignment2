## Improve performance of martix inverse computation by caching
# Example/Sample 

# x <- rbind(c(1,-1,-1), c(-1,2,3),c(1,1,4))
# c<-makeCacheMatrix(x)
# c$get()
# cacheSolve(c)  Cache gets generated-first call
# cacheSolve(c)  - returned from cache

# makeCacheMatrix creates a list containing a function to
# * set the value of the matrix
# * get the value of the matrix
# * set the value of inverse of the matrix
# * get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
  {
  
    inverse <- NULL
    set <- function(y) 
      {
         x <<- y
         inverse <<- NULL
      }
    
    get <- function() x
    setinverse <- function(inverseMartix) inverse <<- inverseMartix
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

  }


## Function verifies if inverse of the martix has already being computed
## if so it returns the result from Cache
## else if computes the inverse, caches it and returns the result

cacheSolve <- function(x, ...) 
  {
          
    result <- x$getinverse()
    
    if(!is.null(result)) 
      {
         message("getting inverse from cached data")
         return(result)
      }
    
    data <- x$get()
    result <- solve(data)  
    x$setinverse(result)
    result
  
  }
