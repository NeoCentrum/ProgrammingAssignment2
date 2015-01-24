##Data Science R Programming Course Assignment 2  
# Objective: Enable Efficiency during Matrix Inverse Computations by Caching Matrix Inverse.
##DEscription : functions enable storage of the inverse of input matrix in 
#a cache. This cache can be used to fetch cached inverse of an unchanged matrix.


## makeCacheMatrix(): is used to initiate a Object Storage which can cache inverse of a matrix. 
#input parameters: 'x' the matrix whose inverse is to be cached.
makeCacheMatrix <- function(x = matrix()) {
     #we store cached mean in this.
     cachedinvrse <- NULL
     #reset the prev stored matrix obj key (x)
     set <- function(cmat) {
       x <<- cmat
       cachedinvrse <<- NULL
     }
     #get the prev stored matrix obj key(x)
     get <- function() x
     #set mean of the matrix obj key(x) 
     setinvrse <- function(invrse) cachedinvrse <<- invrse
     #get the cached mean for matrix obj key(x)
     getinvrse <- function() cachedinvrse
     #list of available obj attribs and functions
     list(set=set,get=get,setinvrse=setinvrse,getinvrse=getinvrse)
}


## cacheResolve(): is used to calculate inverse of a matrix object, if stored using makeCacheMatrix(x) function then the cached value is used else
#computed value is cached. 
#input parameters: 'x' the matrix whose inverse is to be cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrse <- x$getinvrse()
  if(!is.null(invrse)) {
    message("getting cached data")
    return(cachedinvrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinvrse(invrse)
  invrse
}
