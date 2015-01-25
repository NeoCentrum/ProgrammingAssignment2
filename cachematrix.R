##Data Science R Programming Course Assignment 2  
# Objective: Enable Efficiency during Matrix Inverse Computations by Caching Matrix Inverse.
##Description : functions enable storage of the inverse of input matrix in 
#               a cache. This cache can be used to fetch cached inverse of an unchanged matrix.


#makeCacheMatrix(): is used to initiate a Object Storage which can cache inverse of a matrix(assumption- an invertible matrix is provided). 
#input parameters: 'x' the matrix whose inverse is to be cached.
#return : returns an special Matrix Object attributes list(member functions)
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
     #set inverse of the matrix obj key(x) 
     setinvrse <- function(invrse) cachedinvrse <<- invrse
     #get the cached inverse for matrix obj key(x)
     getinvrse <- function() cachedinvrse
     #list of available obj attribs and functions
     list(set=set,get=get,setinvrse=setinvrse,getinvrse=getinvrse)
}


## cacheSolve(): is used to calculate inverse of the special matrix object created using makeCacheMatrix(x) function 
#                if the matrix is not previously cached, the inverse is computed and stored, else cached value is returned. 
#input parameters: 'x' the special matrix created using makeCacheMatrix() function, whose inverse is to be computed
#return: inverse of the input matrix.
cacheSolve <- function(x, ...) {
  #check if inverse is cache before
  cachedinvrse <- x$getinvrse()
  if(!is.null(cachedinvrse)) {
    message("getting cached inverse")
    return(cachedinvrse)
  }
  #inverse not found in cache, compute inverse and store in cache
  data <- x$get()
  #compute inverse
  invrse <- solve(data, ...)
  #store inverse in cache
  x$setinvrse(invrse)
  #return inverse
  invrse
}
